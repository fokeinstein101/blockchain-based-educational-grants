;; Grant Disbursement Smart Contract
;; Releases funds to verified recipients based on milestones and compliance
;; Manages automated payments and progress tracking for educational grants

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u800))
(define-constant ERR-DISBURSEMENT-NOT-FOUND (err u801))
(define-constant ERR-INSUFFICIENT-FUNDS (err u802))
(define-constant ERR-INVALID-AMOUNT (err u803))
(define-constant ERR-ALREADY-DISBURSED (err u804))
(define-constant ERR-MILESTONE-NOT-MET (err u805))
(define-constant ERR-GRANT-EXPIRED (err u806))

;; Disbursement Status
(define-constant DISBURSEMENT-PENDING u1)
(define-constant DISBURSEMENT-APPROVED u2)
(define-constant DISBURSEMENT-COMPLETED u3)
(define-constant DISBURSEMENT-CANCELLED u4)

;; Milestone Status
(define-constant MILESTONE-PENDING u1)
(define-constant MILESTONE-COMPLETED u2)
(define-constant MILESTONE-VERIFIED u3)

;; Data Variables
(define-data-var next-disbursement-id uint u1)
(define-data-var total-disbursements uint u0)
(define-data-var total-disbursed-amount uint u0)

;; Data Maps
(define-map grant-disbursements
    uint ;; disbursement-id
    {
        grant-id: uint,
        recipient: principal,
        total-amount: uint,
        disbursed-amount: uint,
        remaining-amount: uint,
        start-date: uint,
        end-date: uint,
        status: uint,
        milestones-count: uint,
        completed-milestones: uint,
        compliance-verified: bool
    }
)

(define-map disbursement-milestones
    {disbursement-id: uint, milestone-id: uint}
    {
        title: (string-ascii 200),
        description: (string-ascii 400),
        amount: uint,
        due-date: uint,
        status: uint,
        completion-date: uint,
        evidence: (string-ascii 300),
        verifier: (optional principal)
    }
)

(define-map payment-schedule
    uint ;; disbursement-id
    (list 20 {
        milestone-id: uint,
        amount: uint,
        due-date: uint,
        paid: bool,
        payment-date: uint
    })
)

(define-map recipient-disbursements
    principal ;; recipient address
    (list 50 uint) ;; list of disbursement IDs
)

(define-map compliance-reports
    {disbursement-id: uint, report-id: uint}
    {
        report-date: uint,
        compliance-status: bool,
        findings: (string-ascii 400),
        auditor: principal,
        next-review-date: uint
    }
)

(define-map fund-utilization
    uint ;; disbursement-id
    (list 50 {
        expense-date: uint,
        amount: uint,
        category: (string-ascii 100),
        description: (string-ascii 200),
        receipt-hash: (string-ascii 64)
    })
)

;; Public Functions

;; Create disbursement plan
(define-public (create-disbursement
    (grant-id uint)
    (recipient principal)
    (total-amount uint)
    (duration-blocks uint)
    (milestones-count uint)
)
    (let (
        (disbursement-id (var-get next-disbursement-id))
        (end-date (+ burn-block-height duration-blocks))
        (recipient-list (default-to (list) (map-get? recipient-disbursements recipient)))
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> total-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> duration-blocks u0) ERR-INVALID-AMOUNT)
        (asserts! (> milestones-count u0) ERR-INVALID-AMOUNT)
        
        ;; Create disbursement record
        (map-set grant-disbursements disbursement-id
            {
                grant-id: grant-id,
                recipient: recipient,
                total-amount: total-amount,
                disbursed-amount: u0,
                remaining-amount: total-amount,
                start-date: burn-block-height,
                end-date: end-date,
                status: DISBURSEMENT-PENDING,
                milestones-count: milestones-count,
                completed-milestones: u0,
                compliance-verified: false
            }
        )
        
        ;; Update recipient's disbursement list
        (map-set recipient-disbursements recipient
            (unwrap! (as-max-len? (append recipient-list disbursement-id) u50) ERR-INVALID-AMOUNT)
        )
        
        ;; Update counters
        (var-set next-disbursement-id (+ disbursement-id u1))
        (var-set total-disbursements (+ (var-get total-disbursements) u1))
        
        (ok disbursement-id)
    )
)

;; Add milestone to disbursement
(define-public (add-milestone
    (disbursement-id uint)
    (milestone-id uint)
    (title (string-ascii 200))
    (description (string-ascii 400))
    (amount uint)
    (due-blocks uint)
)
    (let (
        (disbursement (unwrap! (map-get? grant-disbursements disbursement-id) ERR-DISBURSEMENT-NOT-FOUND))
        (due-date (+ burn-block-height due-blocks))
        (milestone-key {disbursement-id: disbursement-id, milestone-id: milestone-id})
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> (len title) u0) ERR-INVALID-AMOUNT)
        (asserts! (< due-date (get end-date disbursement)) ERR-GRANT-EXPIRED)
        
        ;; Create milestone record
        (map-set disbursement-milestones milestone-key
            {
                title: title,
                description: description,
                amount: amount,
                due-date: due-date,
                status: MILESTONE-PENDING,
                completion-date: u0,
                evidence: "",
                verifier: none
            }
        )
        
        (ok true)
    )
)

;; Submit milestone completion
(define-public (submit-milestone-completion
    (disbursement-id uint)
    (milestone-id uint)
    (evidence (string-ascii 300))
)
    (let (
        (disbursement (unwrap! (map-get? grant-disbursements disbursement-id) ERR-DISBURSEMENT-NOT-FOUND))
        (milestone-key {disbursement-id: disbursement-id, milestone-id: milestone-id})
        (milestone (unwrap! (map-get? disbursement-milestones milestone-key) ERR-DISBURSEMENT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get recipient disbursement)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status milestone) MILESTONE-PENDING) ERR-ALREADY-DISBURSED)
        (asserts! (> (len evidence) u0) ERR-INVALID-AMOUNT)
        
        ;; Update milestone status
        (map-set disbursement-milestones milestone-key
            (merge milestone {
                status: MILESTONE-COMPLETED,
                completion-date: burn-block-height,
                evidence: evidence
            })
        )
        
        (ok true)
    )
)

;; Verify milestone and release funds
(define-public (verify-milestone-and-disburse
    (disbursement-id uint)
    (milestone-id uint)
)
    (let (
        (disbursement (unwrap! (map-get? grant-disbursements disbursement-id) ERR-DISBURSEMENT-NOT-FOUND))
        (milestone-key {disbursement-id: disbursement-id, milestone-id: milestone-id})
        (milestone (unwrap! (map-get? disbursement-milestones milestone-key) ERR-DISBURSEMENT-NOT-FOUND))
        (milestone-amount (get amount milestone))
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status milestone) MILESTONE-COMPLETED) ERR-MILESTONE-NOT-MET)
        (asserts! (<= milestone-amount (get remaining-amount disbursement)) ERR-INSUFFICIENT-FUNDS)
        
        ;; Transfer funds to recipient
        (try! (as-contract (stx-transfer? milestone-amount tx-sender (get recipient disbursement))))
        
        ;; Update milestone status
        (map-set disbursement-milestones milestone-key
            (merge milestone {
                status: MILESTONE-VERIFIED,
                verifier: (some tx-sender)
            })
        )
        
        ;; Update disbursement amounts
        (map-set grant-disbursements disbursement-id
            (merge disbursement {
                disbursed-amount: (+ (get disbursed-amount disbursement) milestone-amount),
                remaining-amount: (- (get remaining-amount disbursement) milestone-amount),
                completed-milestones: (+ (get completed-milestones disbursement) u1)
            })
        )
        
        ;; Update global stats
        (var-set total-disbursed-amount (+ (var-get total-disbursed-amount) milestone-amount))
        
        (ok true)
    )
)

;; Record fund utilization
(define-public (record-expense
    (disbursement-id uint)
    (amount uint)
    (category (string-ascii 100))
    (description (string-ascii 200))
    (receipt-hash (string-ascii 64))
)
    (let (
        (disbursement (unwrap! (map-get? grant-disbursements disbursement-id) ERR-DISBURSEMENT-NOT-FOUND))
        (current-utilization (default-to (list) (map-get? fund-utilization disbursement-id)))
        (expense-record {
            expense-date: burn-block-height,
            amount: amount,
            category: category,
            description: description,
            receipt-hash: receipt-hash
        })
    )
        (asserts! (is-eq tx-sender (get recipient disbursement)) ERR-NOT-AUTHORIZED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> (len category) u0) ERR-INVALID-AMOUNT)
        
        ;; Add expense record
        (map-set fund-utilization disbursement-id
            (unwrap! (as-max-len? (append current-utilization expense-record) u50) ERR-INVALID-AMOUNT)
        )
        
        (ok true)
    )
)

;; Submit compliance report
(define-public (submit-compliance-report
    (disbursement-id uint)
    (report-id uint)
    (compliance-status bool)
    (findings (string-ascii 400))
)
    (let (
        (disbursement (unwrap! (map-get? grant-disbursements disbursement-id) ERR-DISBURSEMENT-NOT-FOUND))
        (report-key {disbursement-id: disbursement-id, report-id: report-id})
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        ;; Create compliance report
        (map-set compliance-reports report-key
            {
                report-date: burn-block-height,
                compliance-status: compliance-status,
                findings: findings,
                auditor: tx-sender,
                next-review-date: (+ burn-block-height u1000) ;; ~7 days
            }
        )
        
        ;; Update disbursement compliance status
        (map-set grant-disbursements disbursement-id
            (merge disbursement {
                compliance-verified: compliance-status
            })
        )
        
        (ok true)
    )
)

;; Read-Only Functions

;; Get disbursement details
(define-read-only (get-disbursement (disbursement-id uint))
    (map-get? grant-disbursements disbursement-id)
)

;; Get milestone details
(define-read-only (get-milestone (disbursement-id uint) (milestone-id uint))
    (map-get? disbursement-milestones {disbursement-id: disbursement-id, milestone-id: milestone-id})
)

;; Get payment schedule
(define-read-only (get-payment-schedule (disbursement-id uint))
    (map-get? payment-schedule disbursement-id)
)

;; Get recipient disbursements
(define-read-only (get-recipient-disbursements (recipient principal))
    (map-get? recipient-disbursements recipient)
)

;; Get fund utilization
(define-read-only (get-fund-utilization (disbursement-id uint))
    (map-get? fund-utilization disbursement-id)
)

;; Get compliance report
(define-read-only (get-compliance-report (disbursement-id uint) (report-id uint))
    (map-get? compliance-reports {disbursement-id: disbursement-id, report-id: report-id})
)

;; Calculate disbursement progress
(define-read-only (calculate-progress (disbursement-id uint))
    (match (map-get? grant-disbursements disbursement-id)
        disbursement 
        (let (
            (progress-percentage (if (> (get total-amount disbursement) u0)
                (/ (* (get disbursed-amount disbursement) u100) (get total-amount disbursement))
                u0))
            (milestone-progress (if (> (get milestones-count disbursement) u0)
                (/ (* (get completed-milestones disbursement) u100) (get milestones-count disbursement))
                u0))
        )
            {
                financial-progress: progress-percentage,
                milestone-progress: milestone-progress,
                is-compliant: (get compliance-verified disbursement)
            }
        )
        {financial-progress: u0, milestone-progress: u0, is-compliant: false}
    )
)

;; Get disbursement statistics
(define-read-only (get-disbursement-stats)
    {
        total-disbursements: (var-get total-disbursements),
        total-disbursed-amount: (var-get total-disbursed-amount),
        next-disbursement-id: (var-get next-disbursement-id)
    }
)

