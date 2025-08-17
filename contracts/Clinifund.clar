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
(define-constant ERR_NOT_COLLABORATOR (err u116))
(define-constant ERR_COLLABORATION_EXISTS (err u117))
(define-constant ERR_INVALID_SHARE_PERCENTAGE (err u118))
(define-constant ERR_COLLABORATION_NOT_FOUND (err u119))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u120))
(define-constant ERR_ALREADY_VOTED (err u121))
(define-constant ERR_PROPOSAL_EXPIRED (err u122))
(define-constant ERR_INSUFFICIENT_VOTES (err u123))

(define-data-var trial-counter uint u0)
(define-data-var collaboration-counter uint u0)
(define-data-var proposal-counter uint u0)

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

(define-map trial-collaborations
  { trial-id: uint }
  {
    lead-researcher: principal,
    collaboration-id: uint,
    total-collaborators: uint,
    requires-consensus: bool,
    is-active: bool
  }
)

(define-map collaboration-members
  { collaboration-id: uint, member: principal }
  {
    share-percentage: uint,
    role: (string-ascii 50),
    joined-block: uint,
    permissions: uint,
    is-active: bool
  }
)

(define-map collaboration-proposals
  { proposal-id: uint }
  {
    collaboration-id: uint,
    proposer: principal,
    proposal-type: (string-ascii 50),
    description: (string-ascii 300),
    target-member: (optional principal),
    new-value: uint,
    votes-for: uint,
    votes-against: uint,
    votes-required: uint,
    expiry-block: uint,
    is-executed: bool,
    is-active: bool
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  {
    vote: bool,
    vote-block: uint
  }
)

(define-map collaboration-earnings
  { collaboration-id: uint, member: principal }
  {
    total-earned: uint,
    pending-withdrawal: uint,
    last-withdrawal-block: uint
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

(define-public (create-collaboration 
  (trial-id uint)
  (member-addresses (list 10 principal))
  (member-shares (list 10 uint))
  (member-roles (list 10 (string-ascii 50)))
  (requires-consensus bool))
  (let (
    (trial (unwrap! (map-get? trials { trial-id: trial-id }) ERR_TRIAL_NOT_FOUND))
    (collaboration-id (+ (var-get collaboration-counter) u1))
    (total-shares (fold + member-shares u0))
  )
    (asserts! (is-eq tx-sender (get researcher trial)) ERR_NOT_RESEARCHER)
    (asserts! (is-none (map-get? trial-collaborations { trial-id: trial-id })) ERR_COLLABORATION_EXISTS)
    (asserts! (is-eq total-shares u100) ERR_INVALID_SHARE_PERCENTAGE)
    (asserts! (is-eq (len member-addresses) (len member-shares)) ERR_INVALID_SHARE_PERCENTAGE)
    (asserts! (is-eq (len member-addresses) (len member-roles)) ERR_INVALID_SHARE_PERCENTAGE)
    
    (map-set trial-collaborations
      { trial-id: trial-id }
      {
        lead-researcher: tx-sender,
        collaboration-id: collaboration-id,
        total-collaborators: (len member-addresses),
        requires-consensus: requires-consensus,
        is-active: true
      }
    )
    
    (var-set collaboration-counter collaboration-id)
    
    (unwrap! (setup-single-member collaboration-id 
      (default-to 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 (element-at member-addresses u0))
      (default-to u100 (element-at member-shares u0))
      (default-to "Lead" (element-at member-roles u0))) (err u107))
    
    (ok collaboration-id)
  )
)

(define-private (setup-single-member 
  (collaboration-id uint)
  (address principal)
  (share uint)
  (role (string-ascii 50)))
  (begin
    (map-set collaboration-members
      { collaboration-id: collaboration-id, member: address }
      {
        share-percentage: share,
        role: role,
        joined-block: stacks-block-height,
        permissions: u1,
        is-active: true
      }
    )
    (map-set collaboration-earnings
      { collaboration-id: collaboration-id, member: address }
      {
        total-earned: u0,
        pending-withdrawal: u0,
        last-withdrawal-block: u0
      }
    )
    (ok true)
  )
)

(define-public (add-collaborator 
  (trial-id uint)
  (new-member principal)
  (share-percentage uint)
  (role (string-ascii 50)))
  (let (
    (collaboration (unwrap! (map-get? trial-collaborations { trial-id: trial-id }) ERR_COLLABORATION_NOT_FOUND))
    (collaboration-id (get collaboration-id collaboration))
    (member-exists (is-some (map-get? collaboration-members { collaboration-id: collaboration-id, member: new-member })))
  )
    (asserts! (is-collaboration-member collaboration-id tx-sender) ERR_NOT_COLLABORATOR)
    (asserts! (not member-exists) ERR_COLLABORATION_EXISTS)
    (asserts! (> share-percentage u0) ERR_INVALID_SHARE_PERCENTAGE)
    (asserts! (<= share-percentage u100) ERR_INVALID_SHARE_PERCENTAGE)
    
    (if (get requires-consensus collaboration)
      (begin
        (unwrap! (create-governance-proposal 
          collaboration-id 
          "add-member" 
          "Add new collaboration member"
          (some new-member)
          share-percentage) (err u107))
        (ok true)
      )
      (begin
        (asserts! (is-eq tx-sender (get lead-researcher collaboration)) ERR_NOT_AUTHORIZED)
        (unwrap! (execute-add-member collaboration-id new-member share-percentage role) (err u107))
        (ok true)
      )
    )
  )
)

(define-private (execute-add-member 
  (collaboration-id uint)
  (new-member principal)
  (share-percentage uint)
  (role (string-ascii 50)))
  (begin
    (map-set collaboration-members
      { collaboration-id: collaboration-id, member: new-member }
      {
        share-percentage: share-percentage,
        role: role,
        joined-block: stacks-block-height,
        permissions: u1,
        is-active: true
      }
    )
    (map-set collaboration-earnings
      { collaboration-id: collaboration-id, member: new-member }
      {
        total-earned: u0,
        pending-withdrawal: u0,
        last-withdrawal-block: u0
      }
    )
    (ok true)
  )
)

(define-public (create-governance-proposal 
  (collaboration-id uint)
  (proposal-type (string-ascii 50))
  (description (string-ascii 300))
  (target-member (optional principal))
  (new-value uint))
  (let (
    (proposal-id (+ (var-get proposal-counter) u1))
    (collaboration (unwrap! (get-collaboration-by-id collaboration-id) ERR_COLLABORATION_NOT_FOUND))
    (total-members (get total-collaborators collaboration))
    (votes-required (if (> total-members u2) (/ (+ total-members u1) u2) u1))
  )
    (asserts! (is-collaboration-member collaboration-id tx-sender) ERR_NOT_COLLABORATOR)
    
    (map-set collaboration-proposals
      { proposal-id: proposal-id }
      {
        collaboration-id: collaboration-id,
        proposer: tx-sender,
        proposal-type: proposal-type,
        description: description,
        target-member: target-member,
        new-value: new-value,
        votes-for: u0,
        votes-against: u0,
        votes-required: votes-required,
        expiry-block: (+ stacks-block-height u1008),
        is-executed: false,
        is-active: true
      }
    )
    
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote bool))
  (let (
    (proposal (unwrap! (map-get? collaboration-proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
    (existing-vote (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender }))
  )
    (asserts! (get is-active proposal) ERR_PROPOSAL_EXPIRED)
    (asserts! (< stacks-block-height (get expiry-block proposal)) ERR_PROPOSAL_EXPIRED)
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    (asserts! (is-collaboration-member (get collaboration-id proposal) tx-sender) ERR_NOT_COLLABORATOR)
    
    (map-set proposal-votes
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote, vote-block: stacks-block-height }
    )
    
    (let (
      (new-votes-for (if vote (+ (get votes-for proposal) u1) (get votes-for proposal)))
      (new-votes-against (if vote (get votes-against proposal) (+ (get votes-against proposal) u1)))
    )
      (map-set collaboration-proposals
        { proposal-id: proposal-id }
        (merge proposal {
          votes-for: new-votes-for,
          votes-against: new-votes-against
        })
      )
      
      (if (>= new-votes-for (get votes-required proposal))
        (execute-proposal proposal-id)
        (ok true)
      )
    )
  )
)

(define-private (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? collaboration-proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
  )
    (map-set collaboration-proposals
      { proposal-id: proposal-id }
      (merge proposal { is-executed: true, is-active: false })
    )
    
    (if (is-eq (get proposal-type proposal) "add-member")
      (execute-add-member 
        (get collaboration-id proposal)
        (unwrap! (get target-member proposal) ERR_PROPOSAL_NOT_FOUND)
        (get new-value proposal)
        "Member"
      )
      (ok true)
    )
  )
)

(define-public (distribute-milestone-earnings (trial-id uint) (amount uint))
  (let (
    (trial (unwrap! (map-get? trials { trial-id: trial-id }) ERR_TRIAL_NOT_FOUND))
    (collaboration (unwrap! (map-get? trial-collaborations { trial-id: trial-id }) ERR_COLLABORATION_NOT_FOUND))
    (collaboration-id (get collaboration-id collaboration))
  )
    (asserts! (is-collaboration-member collaboration-id tx-sender) ERR_NOT_COLLABORATOR)
    (asserts! (get is-active collaboration) ERR_COLLABORATION_NOT_FOUND)
    
    (unwrap! (distribute-earnings-to-all-members collaboration-id amount) (err u107))
    (ok true)
  )
)

(define-private (distribute-earnings-to-all-members (collaboration-id uint) (total-amount uint))
  (distribute-to-member collaboration-id 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 total-amount)
)

(define-private (distribute-to-member (collaboration-id uint) (member principal) (total-amount uint))
  (match (map-get? collaboration-members { collaboration-id: collaboration-id, member: member })
    member-data
    (let (
      (member-share (get share-percentage member-data))
      (member-amount (/ (* total-amount member-share) u100))
      (current-earnings (default-to 
        { total-earned: u0, pending-withdrawal: u0, last-withdrawal-block: u0 }
        (map-get? collaboration-earnings { collaboration-id: collaboration-id, member: member })
      ))
    )
      (map-set collaboration-earnings
        { collaboration-id: collaboration-id, member: member }
        (merge current-earnings {
          total-earned: (+ (get total-earned current-earnings) member-amount),
          pending-withdrawal: (+ (get pending-withdrawal current-earnings) member-amount)
        })
      )
      (ok true)
    )
    (ok true)
  )
)

(define-public (withdraw-collaboration-earnings (collaboration-id uint))
  (let (
    (earnings (unwrap! (map-get? collaboration-earnings { collaboration-id: collaboration-id, member: tx-sender }) ERR_NOT_COLLABORATOR))
    (withdrawal-amount (get pending-withdrawal earnings))
  )
    (asserts! (> withdrawal-amount u0) ERR_INSUFFICIENT_FUNDS)
    
    (unwrap! (as-contract (stx-transfer? withdrawal-amount tx-sender tx-sender)) (err u107))
    
    (map-set collaboration-earnings
      { collaboration-id: collaboration-id, member: tx-sender }
      (merge earnings {
        pending-withdrawal: u0,
        last-withdrawal-block: stacks-block-height
      })
    )
    
    (ok withdrawal-amount)
  )
)

(define-private (is-collaboration-member (collaboration-id uint) (member principal))
  (match (map-get? collaboration-members { collaboration-id: collaboration-id, member: member })
    member-data (get is-active member-data)
    false
  )
)

(define-private (get-collaboration-by-id (target-collaboration-id uint))
  (match (map-get? trial-collaborations { trial-id: u1 })
    collaboration 
    (some collaboration)
    none
  )
)

(define-read-only (get-collaboration-info (trial-id uint))
  (map-get? trial-collaborations { trial-id: trial-id })
)

(define-read-only (get-collaboration-member (collaboration-id uint) (member principal))
  (map-get? collaboration-members { collaboration-id: collaboration-id, member: member })
)

(define-read-only (get-proposal-info (proposal-id uint))
  (map-get? collaboration-proposals { proposal-id: proposal-id })
)

(define-read-only (get-member-earnings (collaboration-id uint) (member principal))
  (map-get? collaboration-earnings { collaboration-id: collaboration-id, member: member })
)

(define-read-only (get-member-vote (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })
)


