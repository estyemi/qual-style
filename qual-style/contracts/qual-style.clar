;; Photon Quality - Enterprise Blockchain Quality Assurance System
;; A comprehensive quality tracking and validation platform

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-quality-score (err u103))
(define-constant err-insufficient-stake (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-validator-not-active (err u106))

;; Minimum stake required for validators (in microSTX)
(define-constant min-validator-stake u1000000)

;; Data Variables
(define-data-var next-product-id uint u1)
(define-data-var next-validator-id uint u1)

;; Data Maps

;; Product Quality Profile
(define-map products
  { product-id: uint }
  {
    name: (string-ascii 100),
    manufacturer: principal,
    quality-score: uint,
    compliance-status: bool,
    timestamp: uint,
    industry-category: (string-ascii 50),
    is-active: bool
  }
)

;; Quality Assessments
(define-map quality-assessments
  { product-id: uint, assessment-id: uint }
  {
    validator: principal,
    score: uint,
    timestamp: uint,
    notes: (string-ascii 256)
  }
)

;; Validator Registry
(define-map validators
  { validator-id: uint }
  {
    validator-address: principal,
    stake-amount: uint,
    reputation-score: uint,
    total-assessments: uint,
    is-active: bool,
    certification-level: uint
  }
)

;; Validator Address to ID mapping
(define-map validator-addresses
  { address: principal }
  { validator-id: uint }
)

;; Product assessment counter
(define-map product-assessment-count
  { product-id: uint }
  { count: uint }
)

;; Compliance Records
(define-map compliance-records
  { product-id: uint, record-id: uint }
  {
    compliance-type: (string-ascii 50),
    passed: bool,
    timestamp: uint,
    verified-by: principal
  }
)

;; Read-only functions

(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

(define-read-only (get-validator (validator-id uint))
  (map-get? validators { validator-id: validator-id })
)

(define-read-only (get-validator-by-address (address principal))
  (match (map-get? validator-addresses { address: address })
    validator-info (map-get? validators { validator-id: (get validator-id validator-info) })
    none
  )
)

(define-read-only (get-quality-assessment (product-id uint) (assessment-id uint))
  (map-get? quality-assessments { product-id: product-id, assessment-id: assessment-id })
)

(define-read-only (get-assessment-count (product-id uint))
  (default-to { count: u0 } (map-get? product-assessment-count { product-id: product-id }))
)

(define-read-only (get-next-product-id)
  (var-get next-product-id)
)

(define-read-only (get-next-validator-id)
  (var-get next-validator-id)
)

;; Public functions

;; Register a new product
(define-public (register-product (name (string-ascii 100)) (industry-category (string-ascii 50)))
  (let
    (
      (product-id (var-get next-product-id))
    )
    (asserts! (> (len name) u0) err-invalid-quality-score)
    (map-set products
      { product-id: product-id }
      {
        name: name,
        manufacturer: tx-sender,
        quality-score: u100,
        compliance-status: true,
        timestamp: block-height,
        industry-category: industry-category,
        is-active: true
      }
    )
    (map-set product-assessment-count
      { product-id: product-id }
      { count: u0 }
    )
    (var-set next-product-id (+ product-id u1))
    (ok product-id)
  )
)

;; Register as a validator with stake
(define-public (register-validator (stake-amount uint) (certification-level uint))
  (let
    (
      (validator-id (var-get next-validator-id))
    )
    (asserts! (>= stake-amount min-validator-stake) err-insufficient-stake)
    (asserts! (is-none (map-get? validator-addresses { address: tx-sender })) err-already-exists)
    
    ;; Transfer stake to contract
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    (map-set validators
      { validator-id: validator-id }
      {
        validator-address: tx-sender,
        stake-amount: stake-amount,
        reputation-score: u100,
        total-assessments: u0,
        is-active: true,
        certification-level: certification-level
      }
    )
    (map-set validator-addresses
      { address: tx-sender }
      { validator-id: validator-id }
    )
    (var-set next-validator-id (+ validator-id u1))
    (ok validator-id)
  )
)

;; Submit a quality assessment
(define-public (submit-assessment (product-id uint) (score uint) (notes (string-ascii 256)))
  (let
    (
      (validator-info (unwrap! (get-validator-by-address tx-sender) err-unauthorized))
      (product-info (unwrap! (get-product product-id) err-not-found))
      (assessment-count (get count (get-assessment-count product-id)))
      (new-assessment-id (+ assessment-count u1))
    )
    (asserts! (get is-active validator-info) err-validator-not-active)
    (asserts! (get is-active product-info) err-not-found)
    (asserts! (<= score u100) err-invalid-quality-score)
    
    ;; Store assessment
    (map-set quality-assessments
      { product-id: product-id, assessment-id: new-assessment-id }
      {
        validator: tx-sender,
        score: score,
        timestamp: block-height,
        notes: notes
      }
    )
    
    ;; Update assessment count
    (map-set product-assessment-count
      { product-id: product-id }
      { count: new-assessment-id }
    )
    
    ;; Update product quality score (simple average for demonstration)
    (map-set products
      { product-id: product-id }
      (merge product-info { 
        quality-score: score,
        timestamp: block-height
      })
    )
    
    ;; Update validator stats
    (let
      (
        (validator-id (get validator-id (unwrap! (map-get? validator-addresses { address: tx-sender }) err-unauthorized)))
      )
      (map-set validators
        { validator-id: validator-id }
        (merge validator-info {
          total-assessments: (+ (get total-assessments validator-info) u1)
        })
      )
    )
    
    (ok new-assessment-id)
  )
)

;; Update compliance status
(define-public (update-compliance-status (product-id uint) (passed bool) (compliance-type (string-ascii 50)))
  (let
    (
      (product-info (unwrap! (get-product product-id) err-not-found))
      (validator-info (unwrap! (get-validator-by-address tx-sender) err-unauthorized))
    )
    (asserts! (get is-active validator-info) err-validator-not-active)
    (asserts! (get is-active product-info) err-not-found)
    
    ;; Update product compliance status
    (map-set products
      { product-id: product-id }
      (merge product-info { 
        compliance-status: passed,
        timestamp: block-height
      })
    )
    
    (ok true)
  )
)

;; Deactivate a product
(define-public (deactivate-product (product-id uint))
  (let
    (
      (product-info (unwrap! (get-product product-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get manufacturer product-info)) err-unauthorized)
    
    (map-set products
      { product-id: product-id }
      (merge product-info { is-active: false })
    )
    
    (ok true)
  )
)

;; Increase validator stake
(define-public (increase-stake (additional-amount uint))
  (let
    (
      (validator-id (get validator-id (unwrap! (map-get? validator-addresses { address: tx-sender }) err-unauthorized)))
      (validator-info (unwrap! (get-validator validator-id) err-not-found))
    )
    (asserts! (get is-active validator-info) err-validator-not-active)
    
    ;; Transfer additional stake
    (try! (stx-transfer? additional-amount tx-sender (as-contract tx-sender)))
    
    (map-set validators
      { validator-id: validator-id }
      (merge validator-info {
        stake-amount: (+ (get stake-amount validator-info) additional-amount)
      })
    )
    
    (ok true)
  )
)

;; Update validator reputation (admin only for demo)
(define-public (update-validator-reputation (validator-id uint) (new-reputation uint))
  (let
    (
      (validator-info (unwrap! (get-validator validator-id) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-reputation u100) err-invalid-quality-score)
    
    (map-set validators
      { validator-id: validator-id }
      (merge validator-info { reputation-score: new-reputation })
    )
    
    (ok true)
  )
)