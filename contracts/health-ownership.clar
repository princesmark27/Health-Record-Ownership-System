(define-non-fungible-token health-record uint)

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-already-exists (err u101))
(define-constant err-not-found (err u102))
(define-constant err-invalid-access (err u103))

(define-map patient-records
    uint 
    {
        patient: principal,
        doctor: (optional principal),
        hospital: (optional principal),
        record-hash: (string-ascii 64),
        created-at: uint,
        last-updated: uint
    }
)

(define-map access-grants
    { record-id: uint, accessor: principal }
    { granted-by: principal, expires-at: uint }
)

(define-data-var record-id-nonce uint u0)

(define-read-only (get-record-details (record-id uint))
    (match (map-get? patient-records record-id)
        record (ok record)
        (err err-not-found)
    )
)

(define-read-only (check-access (record-id uint) (accessor principal))
    (let ((record (unwrap! (map-get? patient-records record-id) (err err-not-found))))
        (if (or
            (is-eq accessor (get patient record))
            (is-some (map-get? access-grants { record-id: record-id, accessor: accessor }))
        )
            (ok true)
            (err err-invalid-access)
        )
    )
)

(define-public (create-health-record (record-hash (string-ascii 64)))
    (let ((new-id (+ (var-get record-id-nonce) u1)))
        (try! (nft-mint? health-record new-id tx-sender))
        (map-set patient-records
            new-id
            {
                patient: tx-sender,
                doctor: none,
                hospital: none,
                record-hash: record-hash,
                created-at: stacks-block-height,
                last-updated: stacks-block-height
            }
        )
        (var-set record-id-nonce new-id)
        (ok new-id)
    )
)

(define-public (grant-access (record-id uint) (accessor principal) (expires-at uint))
    (let ((record (unwrap! (map-get? patient-records record-id) (err err-not-found))))
        ;; (ok (asserts! (is-eq tx-sender (get patient record)) err-not-authorized))
        (map-set access-grants
            { record-id: record-id, accessor: accessor }
            { granted-by: tx-sender, expires-at: expires-at }
        )
        (ok true)
    )
)

(define-public (revoke-access (record-id uint) (accessor principal))
    (let ((record (unwrap! (map-get? patient-records record-id) (err err-not-found))))
        ;; (asserts! (is-eq tx-sender (get patient record)) err-not-authorized)
        (map-delete access-grants { record-id: record-id, accessor: accessor })
        (ok true)
    )
)

(define-public (update-record (record-id uint) (new-hash (string-ascii 64)))
    (let ((record (unwrap! (map-get? patient-records record-id) (err err-not-found))))
        (try! (check-access record-id tx-sender))
        (map-set patient-records
            record-id
            (merge record {
                record-hash: new-hash,
                last-updated: stacks-block-height
            })
        )
        (ok true)
    )
)

(define-public (transfer-ownership (record-id uint) (new-owner principal))
    (let ((record (unwrap! (map-get? patient-records record-id) (err err-not-found))))
        ;; (try! (nft-transfer? health-record record-id tx-sender new-owner))
        (map-set patient-records
            record-id
            (merge record {
                patient: new-owner,
                last-updated: stacks-block-height
            })
        )
        (ok true)
    )
)



(define-map record-history
    {record-id: uint, version: uint}
    {
        hash: (string-ascii 64),
        modified-by: principal,
        timestamp: uint
    }
)

(define-data-var version-nonce uint u0)

(define-public (get-record-history (record-id uint))
    (ok (map-get? record-history {record-id: record-id, version: (var-get version-nonce)}))
)

(define-public (update-record-with-history (record-id uint) (new-hash (string-ascii 64)))
    (let (
        (record (unwrap! (map-get? patient-records record-id) (err err-not-found)))
        (new-version (+ (var-get version-nonce) u1))
    )
        (try! (check-access record-id tx-sender))
        (map-set record-history
            {record-id: record-id, version: new-version}
            {
                hash: new-hash,
                modified-by: tx-sender,
                timestamp: stacks-block-height
            }
        )
        (var-set version-nonce new-version)
        (try! (update-record record-id new-hash))
        (ok true)
    )
)