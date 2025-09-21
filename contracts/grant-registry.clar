;; Grant Registry Smart Contract
;; Lists grants and eligibility requirements for educational funding
;; Manages grant programs, applications, and eligibility verification

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u700))
(define-constant ERR-GRANT-NOT-FOUND (err u701))
(define-constant ERR-INVALID-INPUT (err u702))
(define-constant ERR-ALREADY-APPLIED (err u703))
(define-constant ERR-NOT-ELIGIBLE (err u704))
(define-constant ERR-APPLICATION-CLOSED (err u705))

;; Grant Status
(define-constant GRANT-ACTIVE u1)
(define-constant GRANT-CLOSED u2)
(define-constant GRANT-SUSPENDED u3)

;; Application Status
(define-constant APP-SUBMITTED u1)
(define-constant APP-UNDER-REVIEW u2)
(define-constant APP-APPROVED u3)
(define-constant APP-REJECTED u4)

;; Data Variables
(define-data-var next-grant-id uint u1)
(define-data-var next-application-id uint u1)
(define-data-var total-grants uint u0)
(define-data-var total-applications uint u0)

;; Data Maps
(define-map grant-programs
    uint ;; grant-id
    {
        title: (string-ascii 200),
        description: (string-ascii 500),
        provider: principal,
        total-funding: uint,
        remaining-funding: uint,
        min-grant-amount: uint,
        max-grant-amount: uint,
        application-deadline: uint,
        eligibility-criteria: (string-ascii 400),
        required-documents: (string-ascii 300),
        status: uint,
        creation-date: uint,
        category: (string-ascii 100)
    }
)

(define-map grant-applications
    uint ;; application-id
    {
        grant-id: uint,
        applicant: principal,
        requested-amount: uint,
        application-data: (string-ascii 500),
        supporting-documents: (string-ascii 200),
        submission-date: uint,
        review-date: uint,
        status: uint,
        reviewer: (optional principal),
        review-notes: (string-ascii 300)
    }
)

(define-map eligibility-requirements
    uint ;; grant-id
    (list 10 {
        requirement-type: (string-ascii 50),
        description: (string-ascii 200),
        mandatory: bool
    })
)

(define-map applicant-profiles
    principal ;; applicant address
    {
        name: (string-ascii 100),
        institution: (string-ascii 100),
        field-of-study: (string-ascii 100),
        academic-level: (string-ascii 50),
        gpa: uint,
        verified: bool,
        total-applications: uint,
        approved-grants: uint
    }
)

(define-map provider-grants
    principal ;; provider address
    (list 50 uint) ;; list of grant IDs
)

(define-map applicant-applications
    principal ;; applicant address
    (list 100 uint) ;; list of application IDs
)

;; Public Functions

;; Create new grant program
(define-public (create-grant-program
    (title (string-ascii 200))
    (description (string-ascii 500))
    (total-funding uint)
    (min-amount uint)
    (max-amount uint)
    (deadline-blocks uint)
    (eligibility-criteria (string-ascii 400))
    (category (string-ascii 100))
)
    (let (
        (grant-id (var-get next-grant-id))
        (provider-grants-list (default-to (list) (map-get? provider-grants tx-sender)))
        (application-deadline (+ burn-block-height deadline-blocks))
    )
        (asserts! (> total-funding u0) ERR-INVALID-INPUT)
        (asserts! (> max-amount min-amount) ERR-INVALID-INPUT)
        (asserts! (> deadline-blocks u0) ERR-INVALID-INPUT)
        (asserts! (> (len title) u0) ERR-INVALID-INPUT)
        
        ;; Create grant program
        (map-set grant-programs grant-id
            {
                title: title,
                description: description,
                provider: tx-sender,
                total-funding: total-funding,
                remaining-funding: total-funding,
                min-grant-amount: min-amount,
                max-grant-amount: max-amount,
                application-deadline: application-deadline,
                eligibility-criteria: eligibility-criteria,
                required-documents: "Academic transcripts, recommendation letters",
                status: GRANT-ACTIVE,
                creation-date: burn-block-height,
                category: category
            }
        )
        
        ;; Update provider's grant list
        (map-set provider-grants tx-sender
            (unwrap! (as-max-len? (append provider-grants-list grant-id) u50) ERR-INVALID-INPUT)
        )
        
        ;; Update counters
        (var-set next-grant-id (+ grant-id u1))
        (var-set total-grants (+ (var-get total-grants) u1))
        
        (ok grant-id)
    )
)

;; Submit grant application
(define-public (submit-application
    (grant-id uint)
    (requested-amount uint)
    (application-data (string-ascii 500))
    (supporting-documents (string-ascii 200))
)
    (let (
        (grant (unwrap! (map-get? grant-programs grant-id) ERR-GRANT-NOT-FOUND))
        (application-id (var-get next-application-id))
        (applicant-apps (default-to (list) (map-get? applicant-applications tx-sender)))
    )
        (asserts! (is-eq (get status grant) GRANT-ACTIVE) ERR-APPLICATION-CLOSED)
        (asserts! (< burn-block-height (get application-deadline grant)) ERR-APPLICATION-CLOSED)
        (asserts! (>= requested-amount (get min-grant-amount grant)) ERR-INVALID-INPUT)
        (asserts! (<= requested-amount (get max-grant-amount grant)) ERR-INVALID-INPUT)
        (asserts! (> (len application-data) u0) ERR-INVALID-INPUT)
        
        ;; Create application record
        (map-set grant-applications application-id
            {
                grant-id: grant-id,
                applicant: tx-sender,
                requested-amount: requested-amount,
                application-data: application-data,
                supporting-documents: supporting-documents,
                submission-date: burn-block-height,
                review-date: u0,
                status: APP-SUBMITTED,
                reviewer: none,
                review-notes: ""
            }
        )
        
        ;; Update applicant's application list
        (map-set applicant-applications tx-sender
            (unwrap! (as-max-len? (append applicant-apps application-id) u100) ERR-INVALID-INPUT)
        )
        
        ;; Update applicant profile
        (let (
            (current-profile (default-to {
                name: "",
                institution: "",
                field-of-study: "",
                academic-level: "",
                gpa: u0,
                verified: false,
                total-applications: u0,
                approved-grants: u0
            } (map-get? applicant-profiles tx-sender)))
        )
            (map-set applicant-profiles tx-sender
                (merge current-profile {
                    total-applications: (+ (get total-applications current-profile) u1)
                })
            )
        )
        
        ;; Update counters
        (var-set next-application-id (+ application-id u1))
        (var-set total-applications (+ (var-get total-applications) u1))
        
        (ok application-id)
    )
)

;; Review application
(define-public (review-application
    (application-id uint)
    (decision uint)
    (review-notes (string-ascii 300))
)
    (let (
        (application (unwrap! (map-get? grant-applications application-id) ERR-GRANT-NOT-FOUND))
        (grant (unwrap! (map-get? grant-programs (get grant-id application)) ERR-GRANT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get provider grant)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status application) APP-SUBMITTED) ERR-ALREADY-APPLIED)
        (asserts! (<= decision u4) ERR-INVALID-INPUT)
        
        ;; Update application with review
        (map-set grant-applications application-id
            (merge application {
                status: decision,
                review-date: burn-block-height,
                reviewer: (some tx-sender),
                review-notes: review-notes
            })
        )
        
        ;; If approved, update grant funding and applicant stats
        (if (is-eq decision APP-APPROVED)
            (begin
                (map-set grant-programs (get grant-id application)
                    (merge grant {
                        remaining-funding: (- (get remaining-funding grant) (get requested-amount application))
                    })
                )
                
                ;; Update applicant profile
                (let (
                    (applicant-profile (unwrap! (map-get? applicant-profiles (get applicant application)) ERR-GRANT-NOT-FOUND))
                )
                    (map-set applicant-profiles (get applicant application)
                        (merge applicant-profile {
                            approved-grants: (+ (get approved-grants applicant-profile) u1)
                        })
                    )
                )
            )
            true
        )
        
        (ok true)
    )
)

;; Update applicant profile
(define-public (update-profile
    (name (string-ascii 100))
    (institution (string-ascii 100))
    (field-of-study (string-ascii 100))
    (academic-level (string-ascii 50))
    (gpa uint)
)
    (let (
        (current-profile (default-to {
            name: "",
            institution: "",
            field-of-study: "",
            academic-level: "",
            gpa: u0,
            verified: false,
            total-applications: u0,
            approved-grants: u0
        } (map-get? applicant-profiles tx-sender)))
    )
        (asserts! (> (len name) u0) ERR-INVALID-INPUT)
        (asserts! (<= gpa u400) ERR-INVALID-INPUT) ;; Max GPA 4.00 * 100
        
        (map-set applicant-profiles tx-sender
            (merge current-profile {
                name: name,
                institution: institution,
                field-of-study: field-of-study,
                academic-level: academic-level,
                gpa: gpa
            })
        )
        
        (ok true)
    )
)

;; Update grant status
(define-public (update-grant-status (grant-id uint) (new-status uint))
    (let (
        (grant (unwrap! (map-get? grant-programs grant-id) ERR-GRANT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get provider grant)) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-status u3) ERR-INVALID-INPUT)
        
        (map-set grant-programs grant-id
            (merge grant {status: new-status})
        )
        
        (ok true)
    )
)

;; Read-Only Functions

;; Get grant program details
(define-read-only (get-grant-program (grant-id uint))
    (map-get? grant-programs grant-id)
)

;; Get application details
(define-read-only (get-application (application-id uint))
    (map-get? grant-applications application-id)
)

;; Get applicant profile
(define-read-only (get-applicant-profile (applicant principal))
    (map-get? applicant-profiles applicant)
)

;; Get provider's grants
(define-read-only (get-provider-grants (provider principal))
    (map-get? provider-grants provider)
)

;; Get applicant's applications
(define-read-only (get-applicant-applications (applicant principal))
    (map-get? applicant-applications applicant)
)

;; Get eligibility requirements
(define-read-only (get-eligibility-requirements (grant-id uint))
    (map-get? eligibility-requirements grant-id)
)

;; Check if grant is active
(define-read-only (is-grant-active (grant-id uint))
    (match (map-get? grant-programs grant-id)
        grant (and
            (is-eq (get status grant) GRANT-ACTIVE)
            (> (get application-deadline grant) burn-block-height)
            (> (get remaining-funding grant) u0)
        )
        false
    )
)

;; Get system statistics
(define-read-only (get-system-stats)
    {
        total-grants: (var-get total-grants),
        total-applications: (var-get total-applications),
        next-grant-id: (var-get next-grant-id),
        next-application-id: (var-get next-application-id)
    }
)

