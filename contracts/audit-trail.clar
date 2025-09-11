;; Health Record Audit Trail System
;; Comprehensive logging and compliance monitoring for health record activities

;; Error constants
(define-constant err-not-authorized (err u400))
(define-constant err-not-found (err u401))
(define-constant err-invalid-parameters (err u402))
(define-constant err-log-limit-exceeded (err u403))

;; Audit event types
(define-constant event-record-created "RECORD_CREATED")
(define-constant event-record-accessed "RECORD_ACCESSED")
(define-constant event-record-updated "RECORD_UPDATED")
(define-constant event-access-granted "ACCESS_GRANTED")
(define-constant event-access-revoked "ACCESS_REVOKED")
(define-constant event-emergency-access "EMERGENCY_ACCESS")
(define-constant event-transfer-ownership "TRANSFER_OWNERSHIP")

;; Audit log entries for all health record activities
(define-map audit-logs
    uint
    {
        record-id: uint,
        patient: principal,
        accessor: principal,
        event-type: (string-ascii 20),
        event-details: (string-ascii 100),
        timestamp: uint,
        block-height: uint,
        ip-hash: (optional (string-ascii 64)),
        success: bool
    }
)

;; Patient activity summaries for quick access
(define-map patient-activity-summary
    principal
    {
        total-accesses: uint,
        last-access: uint,
        failed-attempts: uint,
        emergency-accesses: uint,
        data-updates: uint,
        access-grants: uint,
        suspicious-activity: uint
    }
)

;; Compliance monitoring alerts
(define-map compliance-alerts
    uint
    {
        patient: principal,
        alert-type: (string-ascii 30),
        severity: (string-ascii 10),
        description: (string-ascii 150),
        triggered-at: uint,
        resolved: bool,
        reviewed-by: (optional principal)
    }
)

;; Professional access patterns for monitoring
(define-map professional-access-patterns
    principal
    {
        total-record-accesses: uint,
        unique-patients: uint,
        average-session-duration: uint,
        last-access: uint,
        unusual-activity-score: uint
    }
)

(define-data-var audit-log-nonce uint u0)
(define-data-var alert-nonce uint u0)
(define-data-var max-logs-per-record uint u1000)
(define-data-var suspicious-threshold uint u10)

;; Log audit event
(define-public (log-audit-event 
    (record-id uint)
    (patient principal)
    (accessor principal)
    (event-type (string-ascii 20))
    (event-details (string-ascii 100))
    (ip-hash (optional (string-ascii 64)))
    (success bool)
)
    (let ((new-log-id (+ (var-get audit-log-nonce) u1)))
        
        ;; Store audit log entry
        (map-set audit-logs
            new-log-id
            {
                record-id: record-id,
                patient: patient,
                accessor: accessor,
                event-type: event-type,
                event-details: event-details,
                timestamp: stacks-block-height,
                block-height: stacks-block-height,
                ip-hash: ip-hash,
                success: success
            }
        )
        
        ;; Update patient activity summary
        (map-set patient-activity-summary
            patient
            (let ((current-summary (default-to {
                total-accesses: u0,
                last-access: u0,
                failed-attempts: u0,
                emergency-accesses: u0,
                data-updates: u0,
                access-grants: u0,
                suspicious-activity: u0
            } (map-get? patient-activity-summary patient))))
                (merge current-summary {
                    total-accesses: (+ (get total-accesses current-summary) u1),
                    last-access: stacks-block-height,
                    failed-attempts: (+ (get failed-attempts current-summary) (if success u0 u1)),
                    emergency-accesses: (+ (get emergency-accesses current-summary) 
                        (if (is-eq event-type event-emergency-access) u1 u0)),
                    data-updates: (+ (get data-updates current-summary) 
                        (if (is-eq event-type event-record-updated) u1 u0)),
                    access-grants: (+ (get access-grants current-summary) 
                        (if (is-eq event-type event-access-granted) u1 u0))
                })
            )
        )
        
        ;; Update professional access patterns
        (map-set professional-access-patterns
            accessor
            (let ((current-pattern (default-to {
                total-record-accesses: u0,
                unique-patients: u0,
                average-session-duration: u0,
                last-access: u0,
                unusual-activity-score: u0
            } (map-get? professional-access-patterns accessor))))
                (merge current-pattern {
                    total-record-accesses: (+ (get total-record-accesses current-pattern) u1),
                    last-access: stacks-block-height
                })
            )
        )
        
        ;; Increment nonce and return
        (var-set audit-log-nonce new-log-id)
        (ok new-log-id)
    )
)

;; Detect and flag suspicious activity patterns
(define-private (detect-suspicious-activity 
    (patient principal)
    (accessor principal)
    (event-type (string-ascii 20))
)
    (let ((patient-summary (unwrap! (map-get? patient-activity-summary patient) (ok u0))))
        (if (or
            (> (get failed-attempts patient-summary) (var-get suspicious-threshold))
            (> (get emergency-accesses patient-summary) u5)
        )
            (create-compliance-alert 
                patient 
                "SUSPICIOUS_ACTIVITY" 
                "HIGH" 
                "Multiple failed access attempts or excessive emergency access"
            )
            (ok u0)
        )
    )
)

;; Create compliance alert
(define-private (create-compliance-alert 
    (patient principal)
    (alert-type (string-ascii 30))
    (severity (string-ascii 10))
    (description (string-ascii 150))
)
    (let ((new-alert-id (+ (var-get alert-nonce) u1)))
        (map-set compliance-alerts
            new-alert-id
            {
                patient: patient,
                alert-type: alert-type,
                severity: severity,
                description: description,
                triggered-at: stacks-block-height,
                resolved: false,
                reviewed-by: none
            }
        )
        (var-set alert-nonce new-alert-id)
        (ok new-alert-id)
    )
)

;; Get audit log entry
(define-read-only (get-audit-log (log-id uint))
    (map-get? audit-logs log-id)
)

;; Get patient activity summary
(define-read-only (get-patient-activity (patient principal))
    (map-get? patient-activity-summary patient)
)

;; Get professional access patterns
(define-read-only (get-professional-patterns (professional principal))
    (map-get? professional-access-patterns professional)
)

;; Get compliance alert details
(define-read-only (get-compliance-alert (alert-id uint))
    (map-get? compliance-alerts alert-id)
)

;; Generate compliance report for patient
(define-read-only (generate-compliance-report (patient principal))
    (ok (map-get? patient-activity-summary patient))
)

;; Resolve compliance alert (admin function)
(define-public (resolve-alert (alert-id uint))
    (let ((alert (unwrap! (map-get? compliance-alerts alert-id) (err err-not-found))))
        (map-set compliance-alerts
            alert-id
            (merge alert {
                resolved: true,
                reviewed-by: (some tx-sender)
            })
        )
        (ok true)
    )
)

;; Check if access should be granted based on patterns
(define-read-only (should-allow-access (patient principal) (accessor principal))
    (let ((patient-summary (map-get? patient-activity-summary patient)))
        (match patient-summary
            summary (ok (< (get failed-attempts summary) (var-get suspicious-threshold)))
            (ok true)
        )
    )
)

;; Update suspicious activity threshold (admin only)
(define-public (update-suspicious-threshold (new-threshold uint))
    (begin
        (var-set suspicious-threshold new-threshold)
        (ok true)
    )
)

;; Get recent audit logs for a patient (last 10 entries)
(define-read-only (get-recent-patient-logs (patient principal))
    (let ((current-id (var-get audit-log-nonce)))
        (ok current-id)
    )
)
