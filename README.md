# Health Record Ownership System

A decentralized system for managing medical record ownership and access control using NFTs on Stacks blockchain.

## Features

- Create health records as NFTs
- Grant/revoke access to specific providers
- Transfer record ownership
- Update record content
- Access control verification

## Contract Functions

### Patient Functions

- `create-health-record`: Create a new health record NFT
- `grant-access`: Grant access to a healthcare provider
- `revoke-access`: Revoke previously granted access
- `transfer-ownership`: Transfer record ownership to another patient
- `update-record`: Update record content hash

### Read-Only Functions

- `get-record-details`: View record metadata
- `check-access`: Verify access permissions

## Usage Example

```clarity
;; Create a new health record
(contract-call? .health-ownership create-health-record "QmHash...")

;; Grant access to doctor
(contract-call? .health-ownership grant-access u1 'DOCTOR_ADDRESS u100)

;; Update record
(contract-call? .health-ownership update-record u1 "QmNewHash...")
```

## Security

- Only record owners can grant/revoke access
- Access grants can be time-limited
- All operations verify proper authorization
```
