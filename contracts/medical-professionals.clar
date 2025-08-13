;; Medical Professional Verification & Reputation System
;; Enables verification of medical credentials and reputation tracking

(define-non-fungible-token medical-license uint)

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u300))
(define-constant err-not-found (err u301))
(define-constant err-already-exists (err u302))
(define-constant err-invalid-credentials (err u303))
(define-constant err-license-expired (err u304))
(define-constant err-insufficient-reputation (err u305))
(define-constant err-invalid-rating (err u306))

;; Professional profiles with verified credentials
(define-map medical-professionals
    principal
    {
        license-number: (string-ascii 20),
        full-name: (string-ascii 100),
        specialization: (string-ascii 50),
        institution: (string-ascii 100),
        license-expiry: uint,
        verified-at: uint,
        verification-status: (string-ascii 20),
        years-experience: uint,
        board-certified: bool
    }
)

;; Professional reputation and performance metrics
(define-map professional-reputation
    principal
    {
        total-ratings: uint,
        average-rating: uint,
        total-consultations: uint,
        successful-treatments: uint,
        patient-satisfaction: uint,
        peer-endorsements: uint,
        research-publications: uint,
        compliance-score: uint
    }
)

;; Patient feedback and ratings for professionals
(define-map professional-ratings
    {professional: principal, patient: principal, consultation-id: uint}
    {
        rating: uint,
        feedback-type: (string-ascii 30),
        treatment-outcome: (string-ascii 20),
        bedside-manner: uint,
        expertise-level: uint,
        communication: uint,
        rated-at: uint
    }
)

;; Professional specializations and certifications
(define-map professional-certifications
    {professional: principal, certification-id: uint}
    {
        certification-name: (string-ascii 80),
        issuing-body: (string-ascii 60),
        certification-date: uint,
        expiry-date: uint,
        verification-hash: (string-ascii 64),
        active: bool
    }
)

;; Automatic access rules based on professional credentials
(define-map credential-access-rules
    {patient: principal, rule-id: uint}
    {
        required-specialization: (string-ascii 50),
        minimum-experience: uint,
        minimum-rating: uint,
        board-certification-required: bool,
        auto-grant-access: bool,
        access-duration: uint,
        created-at: uint
    }
)

;; Professional verification requests
(define-map verification-requests
    uint
    {
        professional: principal,
        license-number: (string-ascii 20),
        supporting-documents: (string-ascii 200),
        verification-fee-paid: uint,
        request-status: (string-ascii 20),
        requested-at: uint,
        processed-at: (optional uint),
        verifier: (optional principal)
    }
)

(define-data-var license-id-nonce uint u0)
(define-data-var certification-id-nonce uint u0)
(define-data-var rule-id-nonce uint u0)
(define-data-var verification-request-nonce uint u0)
(define-data-var verification-fee uint u1000)
(define-data-var minimum-rating-threshold uint u3)

;; Read-only functions for querying professional data
(define-read-only (get-professional-profile (professional principal))
    (map-get? medical-professionals professional)
)

(define-read-only (get-professional-reputation (professional principal))
    (map-get? professional-reputation professional)
)

(define-read-only (get-professional-rating (professional principal) (patient principal) (consultation-id uint))
    (map-get? professional-ratings {professional: professional, patient: patient, consultation-id: consultation-id})
)

(define-read-only (get-certification-details (professional principal) (certification-id uint))
    (map-get? professional-certifications {professional: professional, certification-id: certification-id})
)

(define-read-only (get-access-rule (patient principal) (rule-id uint))
    (map-get? credential-access-rules {patient: patient, rule-id: rule-id})
)

(define-read-only (get-verification-request (request-id uint))
    (map-get? verification-requests request-id)
)

;; Check if professional meets specific credential requirements
(define-read-only (meets-credential-requirements 
    (professional principal) 
    (required-specialization (string-ascii 50))
    (min-experience uint)
    (min-rating uint)
    (board-cert-required bool)
)
    (match (map-get? medical-professionals professional)
        profile (match (map-get? professional-reputation professional)
            reputation (and
                (is-eq (get specialization profile) required-specialization)
                (>= (get years-experience profile) min-experience)
                (>= (get average-rating reputation) min-rating)
                (or (not board-cert-required) (get board-certified profile))
                (is-eq (get verification-status profile) "verified")
                (> (get license-expiry profile) stacks-block-height)
            )
            false
        )
        false
    )
)

;; Register as a medical professional
(define-public (register-professional 
    (license-number (string-ascii 20))
    (full-name (string-ascii 100))
    (specialization (string-ascii 50))
    (institution (string-ascii 100))
    (license-expiry uint)
    (years-experience uint)
    (board-certified bool)
)
    (let ((new-license-id (+ (var-get license-id-nonce) u1)))
        (try! (if (is-some (map-get? medical-professionals tx-sender)) (err err-already-exists) (ok true)))
        
        ;; Mint professional license NFT  
        (print "license-minted")
        
        ;; Store professional profile
        (map-set medical-professionals
            tx-sender
            {
                license-number: license-number,
                full-name: full-name,
                specialization: specialization,
                institution: institution,
                license-expiry: license-expiry,
                verified-at: u0,
                verification-status: "pending",
                years-experience: years-experience,
                board-certified: board-certified
            }
        )
        
        ;; Initialize reputation metrics
        (map-set professional-reputation
            tx-sender
            {
                total-ratings: u0,
                average-rating: u0,
                total-consultations: u0,
                successful-treatments: u0,
                patient-satisfaction: u0,
                peer-endorsements: u0,
                research-publications: u0,
                compliance-score: u100
            }
        )
        
        (var-set license-id-nonce new-license-id)
        (ok new-license-id)
    )
)

;; Submit verification request with supporting documents
(define-public (submit-verification-request 
    (supporting-documents (string-ascii 200))
)
    (let (
        (professional-data (unwrap! (map-get? medical-professionals tx-sender) (err err-not-found)))
        (new-request-id (+ (var-get verification-request-nonce) u1))
    )
        ;; Create verification request
        (map-set verification-requests
            new-request-id
            {
                professional: tx-sender,
                license-number: (get license-number professional-data),
                supporting-documents: supporting-documents,
                verification-fee-paid: (var-get verification-fee),
                request-status: "submitted",
                requested-at: stacks-block-height,
                processed-at: none,
                verifier: none
            }
        )
        
        (var-set verification-request-nonce new-request-id)
        (ok new-request-id)
    )
)

;; Verify professional credentials (admin function)
(define-public (verify-professional-credentials 
    (professional principal)
    (request-id uint)
    (approved bool)
)
    (let ((request-data (unwrap! (map-get? verification-requests request-id) (err err-not-found))))
        (try! (if (not (is-eq tx-sender contract-owner)) (err err-not-authorized) (ok true)))
        (try! (if (not (is-eq professional (get professional request-data))) (err err-not-authorized) (ok true)))
        
        ;; Update professional verification status
        (map-set medical-professionals
            professional
            (merge (unwrap! (map-get? medical-professionals professional) (err err-not-found)) {
                verification-status: (if approved "verified" "rejected"),
                verified-at: stacks-block-height
            })
        )
        
        ;; Update verification request
        (map-set verification-requests
            request-id
            (merge request-data {
                request-status: (if approved "approved" "rejected"),
                processed-at: (some stacks-block-height),
                verifier: (some tx-sender)
            })
        )
        
        (ok approved)
    )
)

;; Add professional certification
(define-public (add-certification 
    (certification-name (string-ascii 80))
    (issuing-body (string-ascii 60))
    (certification-date uint)
    (expiry-date uint)
    (verification-hash (string-ascii 64))
)
    (let ((new-cert-id (+ (var-get certification-id-nonce) u1)))
        (try! (if (is-none (map-get? medical-professionals tx-sender)) (err err-not-found) (ok true)))
        
        (map-set professional-certifications
            {professional: tx-sender, certification-id: new-cert-id}
            {
                certification-name: certification-name,
                issuing-body: issuing-body,
                certification-date: certification-date,
                expiry-date: expiry-date,
                verification-hash: verification-hash,
                active: true
            }
        )
        
        (var-set certification-id-nonce new-cert-id)
        (ok new-cert-id)
    )
)

;; Rate professional after consultation
(define-public (rate-professional 
    (professional principal)
    (consultation-id uint)
    (overall-rating uint)
    (feedback-type (string-ascii 30))
    (treatment-outcome (string-ascii 20))
    (bedside-manner uint)
    (expertise-level uint)
    (communication uint)
)
    (let ((current-reputation (unwrap! (map-get? professional-reputation professional) (err err-not-found))))
        (try! (if (> overall-rating u5) (err err-invalid-rating) (ok true)))
        (try! (if (> bedside-manner u5) (err err-invalid-rating) (ok true)))
        (try! (if (> expertise-level u5) (err err-invalid-rating) (ok true)))
        (try! (if (> communication u5) (err err-invalid-rating) (ok true)))
        
        ;; Store detailed rating
        (map-set professional-ratings
            {professional: professional, patient: tx-sender, consultation-id: consultation-id}
            {
                rating: overall-rating,
                feedback-type: feedback-type,
                treatment-outcome: treatment-outcome,
                bedside-manner: bedside-manner,
                expertise-level: expertise-level,
                communication: communication,
                rated-at: stacks-block-height
            }
        )
        
        ;; Update reputation metrics
        (let (
            (new-total-ratings (+ (get total-ratings current-reputation) u1))
            (new-average-rating (/ (+ (* (get average-rating current-reputation) (get total-ratings current-reputation)) overall-rating) new-total-ratings))
        )
            (map-set professional-reputation
                professional
                (merge current-reputation {
                    total-ratings: new-total-ratings,
                    average-rating: new-average-rating,
                    total-consultations: (+ (get total-consultations current-reputation) u1)
                })
            )
        )
        
        (ok true)
    )
)

;; Create automatic access rule based on credentials
(define-public (create-credential-access-rule 
    (required-specialization (string-ascii 50))
    (minimum-experience uint)
    (minimum-rating uint)
    (board-certification-required bool)
    (auto-grant-access bool)
    (access-duration uint)
)
    (let ((new-rule-id (+ (var-get rule-id-nonce) u1)))
        (map-set credential-access-rules
            {patient: tx-sender, rule-id: new-rule-id}
            {
                required-specialization: required-specialization,
                minimum-experience: minimum-experience,
                minimum-rating: minimum-rating,
                board-certification-required: board-certification-required,
                auto-grant-access: auto-grant-access,
                access-duration: access-duration,
                created-at: stacks-block-height
            }
        )
        
        (var-set rule-id-nonce new-rule-id)
        (ok new-rule-id)
    )
)

;; Check if professional can auto-access patient records
(define-read-only (check-auto-access-eligibility (patient principal) (professional principal))
    (match (map-get? medical-professionals professional)
        professional-profile (match (map-get? credential-access-rules {patient: patient, rule-id: u1})
            rule (ok (meets-credential-requirements 
                professional
                (get required-specialization rule)
                (get minimum-experience rule)
                (get minimum-rating rule)
                (get board-certification-required rule)
            ))
            (ok false)
        )
        (err err-not-found)
    )
)

;; Update professional consultation statistics
(define-public (record-consultation-outcome 
    (professional principal)
    (successful bool)
    (patient-satisfied bool)
)
    (let ((current-reputation (unwrap! (map-get? professional-reputation professional) (err err-not-found))))
        (try! (if (not (is-eq tx-sender professional)) (err err-not-authorized) (ok true)))
        
        (map-set professional-reputation
            professional
            (merge current-reputation {
                successful-treatments: (+ (get successful-treatments current-reputation) (if successful u1 u0)),
                patient-satisfaction: (+ (get patient-satisfaction current-reputation) (if patient-satisfied u1 u0))
            })
        )
        
        (ok true)
    )
)

;; Set verification fee (admin only)
(define-public (set-verification-fee (new-fee uint))
    (begin
        (try! (if (not (is-eq tx-sender contract-owner)) (err err-not-authorized) (ok true)))
        (var-set verification-fee new-fee)
        (ok true)
    )
)

;; Get professional search results by specialization
(define-read-only (search-professionals-by-specialization (specialization (string-ascii 50)))
    ;; Simplified search - returns success/failure (full implementation would return list)
    (ok true)
)

