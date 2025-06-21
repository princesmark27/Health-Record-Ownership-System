(define-constant err-not-emergency-contact (err u200))
(define-constant err-emergency-not-active (err u201))
(define-constant err-emergency-expired (err u202))
(define-constant err-invalid-emergency-duration (err u203))

(define-map emergency-contacts
    {patient: principal, contact: principal}
    {
        relationship: (string-ascii 50),
        added-at: uint,
        active: bool
    }
)

(define-map active-emergencies
    {patient: principal}
    {
        activated-by: principal,
        activated-at: uint,
        expires-at: uint,
        reason: (string-ascii 100),
        active: bool
    }
)

(define-map emergency-access-log
    {patient: principal, accessor: principal, timestamp: uint}
    {
        record-id: uint,
        emergency-reason: (string-ascii 100),
        access-duration: uint
    }
)

(define-data-var emergency-log-nonce uint u0)

(define-constant max-emergency-duration u144)

(define-public (add-emergency-contact 
    (contact principal) 
    (relationship (string-ascii 50))
)
    (begin
        (map-set emergency-contacts
            {patient: tx-sender, contact: contact}
            {
                relationship: relationship,
                added-at: stacks-block-height,
                active: true
            }
        )
        (ok true)
    )
)

(define-public (remove-emergency-contact (contact principal))
    (begin
        (map-set emergency-contacts
            {patient: tx-sender, contact: contact}
            {
                relationship: "",
                added-at: u0,
                active: false
            }
        )
        (ok true)
    )
)

(define-public (activate-emergency-access 
    (patient principal) 
    (duration uint) 
    (reason (string-ascii 100))
)
    (let ((contact-info (unwrap! (map-get? emergency-contacts {patient: patient, contact: tx-sender}) (err err-not-emergency-contact))))
        (asserts! (get active contact-info) (err err-not-emergency-contact))
        (asserts! (<= duration max-emergency-duration) (err err-invalid-emergency-duration))
        (map-set active-emergencies
            {patient: patient}
            {
                activated-by: tx-sender,
                activated-at: stacks-block-height,
                expires-at: (+ stacks-block-height duration),
                reason: reason,
                active: true
            }
        )
        (ok true)
    )
)

(define-public (deactivate-emergency-access (patient principal))
    (let ((emergency (unwrap! (map-get? active-emergencies {patient: patient}) (err err-emergency-not-active))))
        (map-set active-emergencies
            {patient: patient}
            (merge emergency {active: false})
        )
        (ok true)
    )
)

(define-read-only (check-emergency-access (patient principal) (accessor principal))
    (let (
        (emergency (unwrap! (map-get? active-emergencies {patient: patient}) (err err-emergency-not-active)))
        (contact-info (unwrap! (map-get? emergency-contacts {patient: patient, contact: accessor}) (err err-not-emergency-contact)))
    )
        (asserts! (get active emergency) (err err-emergency-not-active))
        (asserts! (get active contact-info) (err err-not-emergency-contact))
        (asserts! (< stacks-block-height (get expires-at emergency)) (err err-emergency-expired))
        (ok true)
    )
)

(define-public (emergency-access-record (patient principal) (record-id uint))
    (let (
        (log-id (+ (var-get emergency-log-nonce) u1))
        (emergency (unwrap! (map-get? active-emergencies {patient: patient}) (err err-emergency-not-active)))
    )
        (try! (check-emergency-access patient tx-sender))
        (map-set emergency-access-log
            {patient: patient, accessor: tx-sender, timestamp: stacks-block-height}
            {
                record-id: record-id,
                emergency-reason: (get reason emergency),
                access-duration: (- (get expires-at emergency) stacks-block-height)
            }
        )
        (var-set emergency-log-nonce log-id)
        (ok record-id)
    )
)

(define-read-only (get-emergency-contacts (patient principal))
    (ok (map-get? emergency-contacts {patient: patient, contact: tx-sender}))
)

(define-read-only (get-emergency-status (patient principal))
    (match (map-get? active-emergencies {patient: patient})
        emergency (if (and 
                        (get active emergency)
                        (< stacks-block-height (get expires-at emergency))
                     )
                     (ok emergency)
                     (err err-emergency-expired))
        (err err-emergency-not-active)
    )
)

(define-read-only (get-emergency-access-logs (patient principal) (accessor principal))
    (ok (map-get? emergency-access-log {patient: patient, accessor: accessor, timestamp: stacks-block-height}))
)

(define-public (extend-emergency-access (patient principal) (additional-duration uint))
    (let (
        (emergency (unwrap! (map-get? active-emergencies {patient: patient}) (err err-emergency-not-active)))
        (new-expiry (+ (get expires-at emergency) additional-duration))
    )
        (asserts! (is-eq tx-sender (get activated-by emergency)) (err err-not-emergency-contact))
        (asserts! (<= additional-duration max-emergency-duration) (err err-invalid-emergency-duration))
        (map-set active-emergencies
            {patient: patient}
            (merge emergency {expires-at: new-expiry})
        )
        (ok true)
    )
)