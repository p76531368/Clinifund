# 🏥 Clinifund - Medical Research Funding Protocol

A decentralized platform for funding medical research trials with milestone-based payments built on the Stacks blockchain.

## 🎯 Overview

Clinifund enables researchers to create funding campaigns for medical trials with predefined milestones. Funders can contribute STX tokens, and funds are released to researchers only when specific milestones are completed, ensuring accountability and progress tracking.

## ✨ Features

- 🔬 **Trial Creation**: Researchers can create funding campaigns with detailed milestones
- 💰 **Milestone-based Funding**: Funds are released only when milestones are completed
- 🤝 **Community Funding**: Anyone can contribute STX to support medical research
- 🔒 **Secure Escrow**: Funds are held in the contract until milestone completion
- 📊 **Progress Tracking**: Real-time tracking of funding and milestone progress
- 💸 **Refund Protection**: Automatic refunds if funding goals aren't met by deadline

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

```bash
git clone <your-repo>
cd clinifund
clarinet check
```

## 📖 Usage

### For Researchers

#### 1. Create a Trial
```clarity
(contract-call? .clinifund create-trial 
  "Cancer Treatment Study"
  "Phase II clinical trial for new cancer treatment"
  u1000000  ;; 1000 STX funding goal
  (list "Patient Recruitment" "Phase 1 Complete" "Data Analysis")
  (list u300000 u400000 u300000)  ;; Milestone amounts
  u1000)  ;; Funding duration in blocks
```

#### 2. Complete Milestones
```clarity
(contract-call? .clinifund complete-milestone u1 u0)  ;; Complete first milestone
```

### For Funders

#### 1. Fund a Trial
```clarity
(contract-call? .clinifund fund-trial u1 u50000)  ;; Fund 50 STX to trial #1
```

#### 2. Request Refund (if funding fails)
```clarity
(contract-call? .clinifund refund-trial u1)
```

### Read-Only Functions

#### Get Trial Information
```clarity
(contract-call? .clinifund get-trial u1)
```

#### Check Funding Progress
```clarity
(contract-call? .clinifund get-trial-funding-progress u1)
```

#### Check Milestone Progress
```clarity
(contract-call? .clinifund get-trial-milestone-progress u1)
```

## 🏗️ Contract Structure

### Data Maps
- **trials**: Stores trial information and metadata
- **milestones**: Tracks individual milestones for each trial
- **funders**: Records funder contributions
- **researcher-trials**: Maps researchers to their trials

### Key Functions
- `create-trial`: Initialize a new research trial with milestones
- `fund-trial`: Contribute STX to a trial
- `complete-milestone`: Mark milestone as complete and release funds
- `refund-trial`: Claim refund if funding goal not met

## 🔐 Security Features

- ✅ Only researchers can complete their own milestones
- ✅ Funds are held in escrow until milestone completion
- ✅ Automatic refunds for failed funding campaigns
- ✅ Validation of milestone amounts matching funding goals
- ✅ Time-based funding deadlines

## 🧪 Testing

```bash
clarinet test
```

## 📝 Error Codes

| Code | Description |
|------|-------------|
| u100 | Not authorized |
| u101 | Invalid amount |
| u102 | Trial not found |
| u103 | Milestone not found |
| u104 | Insufficient funds |
| u105 | Trial already exists |
| u106 | Milestone already completed |
| u107 | Invalid milestone |
| u108 | Trial completed |
| u109 | Not researcher |
| u110 | Funding period ended |

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

This project is licensed under the MIT License.

## 🌟 Support

If you find this project helpful, please give it a star! ⭐


