(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_TRIAL_NOT_FOUND (err u102))
(define-constant ERR_MILESTONE_NOT_FOUND (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_TRIAL_ALREADY_EXISTS (err u105))
(define-constant ERR_MILESTONE_ALREADY_COMPLETED (err u106))
(define-constant ERR_INVALID_MILESTONE (err u107))
(define-constant ERR_TRIAL_COMPLETED (err u108))
(define-constant ERR_NOT_RESEARCHER (err u109))
(define-constant ERR_FUNDING_PERIOD_ENDED (err u110))

(define-data-var trial-counter uint u0)

(define-map trials
  { trial-id: uint }
  {
    researcher: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    total-funding-goal: uint,
    current-funding: uint,
    milestone-count: uint,
    completed-milestones: uint,
    funding-deadline: uint,
    is-active: bool,
    is-completed: bool
  }
)

(define-map milestones
  { trial-id: uint, milestone-id: uint }
  {
    description: (string-ascii 200),
    funding-amount: uint,
    is-completed: bool,
    completion-block: (optional uint)
  }
)

(define-map funders
  { trial-id: uint, funder: principal }
  { amount: uint }
)

(define-map researcher-trials
  { researcher: principal, trial-id: uint }
  { exists: bool }
)

(define-private (create-milestones 
  (trial-id uint)
  (descriptions (list 10 (string-ascii 200)))
  (amounts (list 10 uint))
  (index uint))
  (match (element-at descriptions index)
    description
    (match (element-at amounts index)
      amount
      (begin
        (map-set milestones
          { trial-id: trial-id, milestone-id: index }
          {
            description: description,
            funding-amount: amount,
            is-completed: false,
            completion-block: none
          }
        )
        (ok true)
      )
      (ok true)
    )
    (ok true)
  )
)

(define-public (create-trial 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (funding-goal uint)
  (milestone-descriptions (list 10 (string-ascii 200)))
  (milestone-amounts (list 10 uint))
  (funding-duration uint))
  (let (
    (trial-id (+ (var-get trial-counter) u1))
    (deadline (+ stacks-block-height funding-duration))
    (milestone-count (len milestone-descriptions))
  )
    (asserts! (> funding-goal u0) ERR_INVALID_AMOUNT)
    (asserts! (> milestone-count u0) ERR_INVALID_MILESTONE)
    (asserts! (is-eq (len milestone-descriptions) (len milestone-amounts)) ERR_INVALID_MILESTONE)
    (asserts! (is-eq (fold + milestone-amounts u0) funding-goal) ERR_INVALID_AMOUNT)
    
    (map-set trials
      { trial-id: trial-id }
      {
        researcher: tx-sender,
        title: title,
        description: description,
        total-funding-goal: funding-goal,
        current-funding: u0,
        milestone-count: milestone-count,
        completed-milestones: u0,
        funding-deadline: deadline,
        is-active: true,
        is-completed: false
      }
    )
    
    (map-set researcher-trials
      { researcher: tx-sender, trial-id: trial-id }
      { exists: true }
    )
    
    (var-set trial-counter trial-id)
    
    (unwrap! (create-milestones trial-id milestone-descriptions milestone-amounts u0) (err u107))
    
    (ok trial-id)
  )
)

(define-public (fund-trial (trial-id uint) (amount uint))
  (let (
    (trial (unwrap! (map-get? trials { trial-id: trial-id }) ERR_TRIAL_NOT_FOUND))
    (current-funder-amount (default-to u0 (get amount (map-get? funders { trial-id: trial-id, funder: tx-sender }))))
  )
    (asserts! (get is-active trial) ERR_TRIAL_NOT_FOUND)
    (asserts! (< stacks-block-height (get funding-deadline trial)) ERR_FUNDING_PERIOD_ENDED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= (+ (get current-funding trial) amount) (get total-funding-goal trial)) ERR_INVALID_AMOUNT)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set trials
      { trial-id: trial-id }
      (merge trial { current-funding: (+ (get current-funding trial) amount) })
    )
    
    (map-set funders
      { trial-id: trial-id, funder: tx-sender }
      { amount: (+ current-funder-amount amount) }
    )
    
    (ok true)
  )
)

(define-public (complete-milestone (trial-id uint) (milestone-id uint))
  (let (
    (trial (unwrap! (map-get? trials { trial-id: trial-id }) ERR_TRIAL_NOT_FOUND))
    (milestone (unwrap! (map-get? milestones { trial-id: trial-id, milestone-id: milestone-id }) ERR_MILESTONE_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get researcher trial)) ERR_NOT_RESEARCHER)
    (asserts! (get is-active trial) ERR_TRIAL_COMPLETED)
    (asserts! (not (get is-completed milestone)) ERR_MILESTONE_ALREADY_COMPLETED)
    (asserts! (>= (get current-funding trial) (get funding-amount milestone)) ERR_INSUFFICIENT_FUNDS)
    
    (try! (as-contract (stx-transfer? (get funding-amount milestone) tx-sender (get researcher trial))))
    
    (map-set milestones
      { trial-id: trial-id, milestone-id: milestone-id }
      (merge milestone {
        is-completed: true,
        completion-block: (some stacks-block-height)
      })
    )
    
    (let ((new-completed-count (+ (get completed-milestones trial) u1)))
      (map-set trials
        { trial-id: trial-id }
        (merge trial {
          completed-milestones: new-completed-count,
          is-completed: (is-eq new-completed-count (get milestone-count trial))
        })
      )
    )
    
    (ok true)
  )
)

(define-public (refund-trial (trial-id uint))
  (let (
    (trial (unwrap! (map-get? trials { trial-id: trial-id }) ERR_TRIAL_NOT_FOUND))
    (funder-info (unwrap! (map-get? funders { trial-id: trial-id, funder: tx-sender }) ERR_INSUFFICIENT_FUNDS))
  )
    (asserts! (> stacks-block-height (get funding-deadline trial)) ERR_FUNDING_PERIOD_ENDED)
    (asserts! (< (get current-funding trial) (get total-funding-goal trial)) ERR_TRIAL_COMPLETED)
    (asserts! (> (get amount funder-info) u0) ERR_INSUFFICIENT_FUNDS)
    
    (try! (as-contract (stx-transfer? (get amount funder-info) tx-sender tx-sender)))
    
    (map-delete funders { trial-id: trial-id, funder: tx-sender })
    
    (ok true)
  )
)

(define-read-only (get-trial (trial-id uint))
  (map-get? trials { trial-id: trial-id })
)

(define-read-only (get-milestone (trial-id uint) (milestone-id uint))
  (map-get? milestones { trial-id: trial-id, milestone-id: milestone-id })
)

(define-read-only (get-funder-contribution (trial-id uint) (funder principal))
  (map-get? funders { trial-id: trial-id, funder: funder })
)

(define-read-only (get-trial-counter)
  (var-get trial-counter)
)

(define-read-only (is-researcher-of-trial (researcher principal) (trial-id uint))
  (is-some (map-get? researcher-trials { researcher: researcher, trial-id: trial-id }))
)

(define-read-only (get-trial-funding-progress (trial-id uint))
  (match (map-get? trials { trial-id: trial-id })
    trial (ok {
      current: (get current-funding trial),
      goal: (get total-funding-goal trial),
      percentage: (/ (* (get current-funding trial) u100) (get total-funding-goal trial))
    })
    ERR_TRIAL_NOT_FOUND
  )
)

(define-read-only (get-trial-milestone-progress (trial-id uint))
  (match (map-get? trials { trial-id: trial-id })
    trial (ok {
      completed: (get completed-milestones trial),
      total: (get milestone-count trial),
      percentage: (/ (* (get completed-milestones trial) u100) (get milestone-count trial))
    })
    ERR_TRIAL_NOT_FOUND
  )
)