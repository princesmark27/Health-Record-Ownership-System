(define-non-fungible-token data-share-agreement uint)

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u200))
(define-constant err-insufficient-payment (err u201))
(define-constant err-invalid-buyer (err u202))
(define-constant err-agreement-expired (err u203))
(define-constant err-already-purchased (err u204))
(define-constant err-not-found (err u205))

(define-map registered-buyers
    principal
    {
        name: (string-ascii 50),
        buyer-type: (string-ascii 20),
        verification-score: uint,
        total-purchases: uint,
        reputation: uint,
        registered-at: uint
    }
)

(define-map data-listings
    uint
    {
        record-id: uint,
        seller: principal,
        price: uint,
        data-type: (string-ascii 30),
        anonymized: bool,
        expires-at: uint,
        max-buyers: uint,
        current-buyers: uint,
        revenue-share: uint
    }
)

(define-map purchase-agreements
    {listing-id: uint, buyer: principal}
    {
        purchased-at: uint,
        access-expires: uint,
        price-paid: uint,
        rating: (optional uint)
    }
)

(define-map seller-earnings
    principal
    {
        total-earned: uint,
        total-sales: uint,
        average-rating: uint
    }
)

(define-data-var listing-id-nonce uint u0)
(define-data-var agreement-id-nonce uint u0)
(define-data-var platform-fee uint u5)

(define-read-only (get-buyer-info (buyer principal))
    (map-get? registered-buyers buyer)
)

(define-read-only (get-listing-details (listing-id uint))
    (map-get? data-listings listing-id)
)

(define-read-only (get-purchase-agreement (listing-id uint) (buyer principal))
    (map-get? purchase-agreements {listing-id: listing-id, buyer: buyer})
)

(define-read-only (get-seller-stats (seller principal))
    (map-get? seller-earnings seller)
)

(define-public (register-buyer (name (string-ascii 50)) (buyer-type (string-ascii 20)))
    (begin
        (map-set registered-buyers
            tx-sender
            {
                name: name,
                buyer-type: buyer-type,
                verification-score: u50,
                total-purchases: u0,
                reputation: u0,
                registered-at: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (create-data-listing 
    (record-id uint)
    (price uint)
    (data-type (string-ascii 30))
    (anonymized bool)
    (duration uint)
    (max-buyers uint)
    (revenue-share uint)
)
    (let ((new-listing-id (+ (var-get listing-id-nonce) u1)))
        (try! (if (> revenue-share u20) (err err-not-authorized) (ok true)))
        (map-set data-listings
            new-listing-id
            {
                record-id: record-id,
                seller: tx-sender,
                price: price,
                data-type: data-type,
                anonymized: anonymized,
                expires-at: (+ stacks-block-height duration),
                max-buyers: max-buyers,
                current-buyers: u0,
                revenue-share: revenue-share
            }
        )
        (var-set listing-id-nonce new-listing-id)
        (ok new-listing-id)
    )
)

(define-public (purchase-data-access (listing-id uint))
    (match (map-get? data-listings listing-id)
        listing (match (map-get? registered-buyers tx-sender)
            buyer-info (let (
                (agreement-id (+ (var-get agreement-id-nonce) u1))
                (platform-fee-amount (/ (* (get price listing) (var-get platform-fee)) u100))
                (seller-amount (- (get price listing) platform-fee-amount))
            )
                (try! (if (>= stacks-block-height (get expires-at listing)) (err err-agreement-expired) (ok true)))
                (try! (if (>= (get current-buyers listing) (get max-buyers listing)) (err err-already-purchased) (ok true)))
                (try! (if (is-some (map-get? purchase-agreements {listing-id: listing-id, buyer: tx-sender})) (err err-already-purchased) (ok true)))
                
                (print "payment-processed")
                
                ;; (try! (nft-mint? data-share-agreement agreement-id tx-sender))
                
                (map-set purchase-agreements
                    {listing-id: listing-id, buyer: tx-sender}
                    {
                        purchased-at: stacks-block-height,
                        access-expires: (+ stacks-block-height u1440),
                        price-paid: (get price listing),
                        rating: none
                    }
                )
                
                (map-set data-listings
                    listing-id
                    (merge listing {current-buyers: (+ (get current-buyers listing) u1)})
                )
                
                (map-set registered-buyers
                    tx-sender
                    (merge buyer-info {total-purchases: (+ (get total-purchases buyer-info) u1)})
                )
                
                (map-set seller-earnings
                    (get seller listing)
                    {
                        total-earned: (+ (get total-earned (default-to {total-earned: u0, total-sales: u0, average-rating: u0} (map-get? seller-earnings (get seller listing)))) seller-amount),
                        total-sales: (+ (get total-sales (default-to {total-earned: u0, total-sales: u0, average-rating: u0} (map-get? seller-earnings (get seller listing)))) u1),
                        average-rating: (get average-rating (default-to {total-earned: u0, total-sales: u0, average-rating: u0} (map-get? seller-earnings (get seller listing))))
                    }
                )
                
                (var-set agreement-id-nonce agreement-id)
                (ok agreement-id)
            )
            (err err-invalid-buyer)
        )
        (err err-not-found)
    )
)

(define-public (rate-data-quality (listing-id uint) (rating uint))
    (let (
        (agreement (unwrap! (map-get? purchase-agreements {listing-id: listing-id, buyer: tx-sender}) (err err-not-found)))
        (listing (unwrap! (map-get? data-listings listing-id) (err err-not-found)))
    )
        (try! (if (> rating u5) (err err-not-authorized) (ok true)))
        (try! (if (< rating u1) (err err-not-authorized) (ok true)))
        (try! (if (is-some (get rating agreement)) (err err-not-authorized) (ok true)))
        
        (map-set purchase-agreements
            {listing-id: listing-id, buyer: tx-sender}
            (merge agreement {rating: (some rating)})
        )
        
        (map-set seller-earnings
            (get seller listing)
            (merge (unwrap! (map-get? seller-earnings (get seller listing)) (err err-not-found)) {
                average-rating: (/ (+ (* (get average-rating (unwrap! (map-get? seller-earnings (get seller listing)) (err err-not-found))) (get total-sales (unwrap! (map-get? seller-earnings (get seller listing)) (err err-not-found)))) rating) (get total-sales (unwrap! (map-get? seller-earnings (get seller listing)) (err err-not-found))))
            })
        )
        
        (ok true)
    )
)

(define-public (update-listing-price (listing-id uint) (new-price uint))
    (let ((listing (unwrap! (map-get? data-listings listing-id) (err err-not-found))))
        (try! (if (not (is-eq tx-sender (get seller listing))) (err err-not-authorized) (ok true)))
        (map-set data-listings
            listing-id
            (merge listing {price: new-price})
        )
        (ok true)
    )
)

(define-public (extend-listing-duration (listing-id uint) (additional-blocks uint))
    (let ((listing (unwrap! (map-get? data-listings listing-id) (err err-not-found))))
        (try! (if (not (is-eq tx-sender (get seller listing))) (err err-not-authorized) (ok true)))
        (map-set data-listings
            listing-id
            (merge listing {expires-at: (+ (get expires-at listing) additional-blocks)})
        )
        (ok true)
    )
)

(define-public (withdraw-earnings)
    (let ((earnings (unwrap! (map-get? seller-earnings tx-sender) (err err-not-found))))
        (try! (if (<= (get total-earned earnings) u0) (err err-not-authorized) (ok true)))
        (print "withdrawal-processed")
        (map-set seller-earnings
            tx-sender
            (merge earnings {total-earned: u0})
        )
        (ok true)
    )
)

(define-public (set-platform-fee (new-fee uint))
    (begin
        (try! (if (not (is-eq tx-sender contract-owner)) (err err-not-authorized) (ok true)))
        (try! (if (> new-fee u10) (err err-not-authorized) (ok true)))
        (var-set platform-fee new-fee)
        (ok true)
    )
)

(define-read-only (calculate-purchase-cost (listing-id uint))
    (match (map-get? data-listings listing-id)
        listing (ok (get price listing))
        (err err-not-found)
    )
)

(define-read-only (get-active-listings)
    (let ((current-height stacks-block-height))
        (ok current-height)
    )
)

(define-read-only (check-access-validity (listing-id uint) (buyer principal))
    (match (map-get? purchase-agreements {listing-id: listing-id, buyer: buyer})
        agreement (ok (< stacks-block-height (get access-expires agreement)))
        (err err-not-found)
    )
)
