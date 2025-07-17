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
(define-constant ERR_RATING_OUT_OF_RANGE (err u111))
(define-constant ERR_REVIEW_EXISTS (err u112))
(define-constant ERR_CANNOT_REVIEW_OWN_TRIAL (err u113))
(define-constant ERR_INSUFFICIENT_CONTRIBUTION (err u114))
(define-constant ERR_TRIAL_NOT_COMPLETED (err u115))

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

(define-map researcher-reputation
  { researcher: principal }
  {
    total-trials: uint,
    completed-trials: uint,
    total-rating: uint,
    review-count: uint,
    average-rating: uint,
    total-funds-raised: uint,
    successful-trials: uint,
    reputation-score: uint
  }
)

(define-map trial-reviews
  { trial-id: uint, reviewer: principal }
  {
    rating: uint,
    review-text: (string-ascii 300),
    review-block: uint,
    contribution-amount: uint
  }
)

(define-map researcher-badges
  { researcher: principal, badge-type: (string-ascii 50) }
  {
    earned: bool,
    earned-block: uint,
    criteria-met: (string-ascii 200)
  }
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
    
    (unwrap! (update-researcher-reputation-on-trial-creation tx-sender funding-goal) (err u107))
    
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
      
      (if (is-eq new-completed-count (get milestone-count trial))
        (update-researcher-reputation-on-completion (get researcher trial) trial-id)
        (ok true)
      )
    )
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

(define-private (update-researcher-reputation-on-trial-creation (researcher principal) (funding-goal uint))
  (let (
    (current-rep (default-to 
      { total-trials: u0, completed-trials: u0, total-rating: u0, review-count: u0, 
        average-rating: u0, total-funds-raised: u0, successful-trials: u0, reputation-score: u0 }
      (map-get? researcher-reputation { researcher: researcher })
    ))
  )
    (map-set researcher-reputation
      { researcher: researcher }
      (merge current-rep {
        total-trials: (+ (get total-trials current-rep) u1),
        total-funds-raised: (+ (get total-funds-raised current-rep) funding-goal)
      })
    )
    (ok true)
  )
)

(define-private (update-researcher-reputation-on-completion (researcher principal) (trial-id uint))
  (let (
    (current-rep (default-to 
      { total-trials: u0, completed-trials: u0, total-rating: u0, review-count: u0, 
        average-rating: u0, total-funds-raised: u0, successful-trials: u0, reputation-score: u0 }
      (map-get? researcher-reputation { researcher: researcher })
    ))
    (trial (unwrap! (map-get? trials { trial-id: trial-id }) ERR_TRIAL_NOT_FOUND))
  )
    (let (
      (new-completed (+ (get completed-trials current-rep) u1))
      (new-successful (if (>= (get current-funding trial) (get total-funding-goal trial))
        (+ (get successful-trials current-rep) u1)
        (get successful-trials current-rep)
      ))
      (new-reputation-score (calculate-reputation-score 
        new-completed 
        (get total-trials current-rep)
        new-successful
        (get average-rating current-rep)
      ))
    )
      (map-set researcher-reputation
        { researcher: researcher }
        (merge current-rep {
          completed-trials: new-completed,
          successful-trials: new-successful,
          reputation-score: new-reputation-score
        })
      )
      (unwrap! (check-and-award-badges researcher new-completed new-successful (get total-funds-raised current-rep)) (err u107))
      (ok true)
    )
  )
)

(define-private (calculate-reputation-score (completed uint) (total uint) (successful uint) (avg-rating uint))
  (let (
    (completion-rate (if (> total u0) (/ (* completed u100) total) u0))
    (success-rate (if (> completed u0) (/ (* successful u100) completed) u0))
    (rating-score (if (> avg-rating u0) (* avg-rating u20) u0))
  )
    (+ completion-rate success-rate rating-score)
  )
)

(define-private (check-and-award-badges (researcher principal) (completed uint) (successful uint) (total-funds uint))
  (begin
    (if (>= completed u5)
      (map-set researcher-badges
        { researcher: researcher, badge-type: "Experienced Researcher" }
        { earned: true, earned-block: stacks-block-height, criteria-met: "Completed 5 or more trials" }
      )
      true
    )
    
    (if (>= successful u3)
      (map-set researcher-badges
        { researcher: researcher, badge-type: "Successful Researcher" }
        { earned: true, earned-block: stacks-block-height, criteria-met: "Successfully funded 3 or more trials" }
      )
      true
    )
    
    (if (>= total-funds u1000000)
      (map-set researcher-badges
        { researcher: researcher, badge-type: "High Volume Researcher" }
        { earned: true, earned-block: stacks-block-height, criteria-met: "Raised over 1,000,000 microSTX in funding" }
      )
      true
    )
    
    (ok true)
  )
)

(define-public (submit-trial-review (trial-id uint) (rating uint) (review-text (string-ascii 300)))
  (let (
    (trial (unwrap! (map-get? trials { trial-id: trial-id }) ERR_TRIAL_NOT_FOUND))
    (funder-contribution (unwrap! (map-get? funders { trial-id: trial-id, funder: tx-sender }) ERR_INSUFFICIENT_CONTRIBUTION))
    (existing-review (map-get? trial-reviews { trial-id: trial-id, reviewer: tx-sender }))
  )
    (asserts! (get is-completed trial) ERR_TRIAL_NOT_COMPLETED)
    (asserts! (not (is-eq tx-sender (get researcher trial))) ERR_CANNOT_REVIEW_OWN_TRIAL)
    (asserts! (>= (get amount funder-contribution) u1000) ERR_INSUFFICIENT_CONTRIBUTION)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_RATING_OUT_OF_RANGE)
    (asserts! (is-none existing-review) ERR_REVIEW_EXISTS)
    
    (map-set trial-reviews
      { trial-id: trial-id, reviewer: tx-sender }
      {
        rating: rating,
        review-text: review-text,
        review-block: stacks-block-height,
        contribution-amount: (get amount funder-contribution)
      }
    )
    
    (unwrap! (update-researcher-rating (get researcher trial) rating) (err u107))
    
    (ok true)
  )
)

(define-private (update-researcher-rating (researcher principal) (new-rating uint))
  (let (
    (current-rep (default-to 
      { total-trials: u0, completed-trials: u0, total-rating: u0, review-count: u0, 
        average-rating: u0, total-funds-raised: u0, successful-trials: u0, reputation-score: u0 }
      (map-get? researcher-reputation { researcher: researcher })
    ))
  )
    (let (
      (new-total-rating (+ (get total-rating current-rep) new-rating))
      (new-review-count (+ (get review-count current-rep) u1))
      (new-average (/ new-total-rating new-review-count))
      (new-reputation-score (calculate-reputation-score 
        (get completed-trials current-rep)
        (get total-trials current-rep)
        (get successful-trials current-rep)
        new-average
      ))
    )
      (map-set researcher-reputation
        { researcher: researcher }
        (merge current-rep {
          total-rating: new-total-rating,
          review-count: new-review-count,
          average-rating: new-average,
          reputation-score: new-reputation-score
        })
      )
      (ok true)
    )
  )
)

(define-read-only (get-researcher-reputation (researcher principal))
  (map-get? researcher-reputation { researcher: researcher })
)

(define-read-only (get-trial-review (trial-id uint) (reviewer principal))
  (map-get? trial-reviews { trial-id: trial-id, reviewer: reviewer })
)

(define-read-only (get-researcher-badge (researcher principal) (badge-type (string-ascii 50)))
  (map-get? researcher-badges { researcher: researcher, badge-type: badge-type })
)

(define-read-only (get-researcher-rank (researcher principal))
  (match (map-get? researcher-reputation { researcher: researcher })
    rep (ok {
      reputation-score: (get reputation-score rep),
      rank: (if (>= (get reputation-score rep) u300) "Expert"
        (if (>= (get reputation-score rep) u200) "Advanced"
          (if (>= (get reputation-score rep) u100) "Intermediate"
            (if (>= (get reputation-score rep) u50) "Novice"
              "Beginner"
            )
          )
        )
      )
    })
    ERR_TRIAL_NOT_FOUND
  )
)

(define-read-only (calculate-funding-bonus (researcher principal) (base-amount uint))
  (match (map-get? researcher-reputation { researcher: researcher })
    rep (let (
      (reputation-score (get reputation-score rep))
      (bonus-multiplier (if (>= reputation-score u300) u120
        (if (>= reputation-score u200) u110
          (if (>= reputation-score u100) u105
            u100
          )
        )
      ))
    )
      (ok (/ (* base-amount bonus-multiplier) u100))
    )
    (ok base-amount)
  )
)