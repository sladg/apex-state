# @sladg/apex-state - Task Breakdown

## 📋 Overview

This directory contains all implementation tasks for the `@sladg/apex-state` package - a powerful valtio wrapper with advanced side-effects and state management.

## 🎯 Task Organization

Tasks are organized into phases with clear dependencies. Each task file contains:
- **Dependencies**: What must be completed first
- **Description**: What needs to be implemented
- **Acceptance Criteria**: Specific requirements for completion
- **Expected Output**: Files/exports that should exist after completion
- **Worker Prompt**: Instructions to keep implementation focused

## 📊 Implementation Phases

### Phase 1: Foundation (P0 - Critical)
**Goal**: Set up project structure, build system, and core type utilities

- `01-project-setup.md` - APEX-1, APEX-2
- `02-core-types.md` - APEX-3, APEX-4, APEX-6, APEX-7

**Completion Criteria**: Package builds, tests run, core types available

### Phase 2: Core Store (P0 - Critical)
**Goal**: Implement base store with valtio integration

- `03-base-store.md` - APEX-10, APEX-11, APEX-12, APEX-13
- `04-basic-hooks.md` - APEX-14, APEX-15, APEX-16

**Completion Criteria**: Store can be created, Provider works, basic hooks functional

### Phase 3: Synchronizer Pipeline (P0 - Critical)
**Goal**: Build the change processing pipeline

- `05-synchronizer-pipeline.md` - APEX-36, APEX-37, APEX-38

**Completion Criteria**: Changes processed through pipeline, atomic updates work

### Phase 4: Side Effects - Core (P1 - High)
**Goal**: Implement most-used side effects (sync, listeners)

- `06-sync-paths.md` - APEX-19, APEX-20, APEX-21
- `07-listeners.md` - APEX-24, APEX-25, APEX-26

**Completion Criteria**: Sync paths work with transitive dependencies, listeners trigger correctly

### Phase 5: Side Effects - Secondary (P1-P2)
**Goal**: Implement remaining side effects

- `08-flip-paths.md` - APEX-22, APEX-23
- `09-aggregations.md` - APEX-31, APEX-32, APEX-33
- `10-validators.md` - APEX-27, APEX-28, APEX-29, APEX-30
- `11-clear-paths.md` - APEX-34, APEX-35

**Completion Criteria**: All side effects implemented and integrated

### Phase 6: Advanced Features (P1-P2)
**Goal**: Enhanced types and form hooks

- `12-advanced-types-hooks.md` - APEX-5, APEX-8, APEX-9, APEX-17, APEX-18

**Completion Criteria**: Advanced type utilities, form field hooks available

### Phase 7: Testing (P1-P2)
**Goal**: Comprehensive use-case testing

- `13-testing.md` - APEX-39 through APEX-46

**Completion Criteria**: All features tested, integration tests pass

### Phase 8: Optimization & Polish (P1-P3)
**Goal**: Performance optimization and documentation

- `14-optimization-docs.md` - APEX-47, APEX-48, APEX-49

**Completion Criteria**: Optimized hot paths, documented APIs

## 🚀 Quick Start for Workers

### 📖 Before You Start - READ THESE:
1. **`00-ARCHITECTURE-UPDATE.md`** ⭐ MANDATORY - Functional patterns, avoid `any`, when to ask
2. **`QUICK-REFERENCE.md`** 📋 Quick lookup for patterns and rules
3. **`IMPLEMENTATION-SUMMARY.md`** 📊 Project overview and timeline

### 🏗️ Implementation Process:
1. **Start with Phase 1**: Always begin with foundational tasks
2. **Check Dependencies**: Only start a task when its dependencies are complete
3. **Read Worker Prompt**: Each task has specific focus instructions
4. **Follow Acceptance Criteria**: These are non-negotiable requirements
5. **Verify Expected Output**: Ensure all specified files/exports exist
6. **Run Tests**: If tests exist, they must pass before marking complete

### ⚠️ CRITICAL RULES:
- ❌ **NO classes** - use factory functions
- ❌ **NO `any`** - use `unknown` or ask for guidance
- ❌ **NO guessing** - STOP and ask when stuck
- ✅ **USE graphology** - for all graph operations
- ✅ **USE remeda pipe** - for multi-step operations
- ✅ **USE @ts-expect-error** - for type error tests

## ⚡ Performance Priorities

**Critical Performance Paths** (optimize heavily):
- Sync paths processing (APEX-21)
- Aggregation processing (APEX-33)
- Change pipeline execution (APEX-37)

These are used on nearly every state change and must be fast.

## 📝 Task Status Tracking

| Phase | Tasks | Status |
|-------|-------|--------|
| Phase 1 | 01-02 | ⬜ Not Started |
| Phase 2 | 03-04 | ⬜ Not Started |
| Phase 3 | 05 | ⬜ Not Started |
| Phase 4 | 06-07 | ⬜ Not Started |
| Phase 5 | 08-11 | ⬜ Not Started |
| Phase 6 | 12 | ⬜ Not Started |
| Phase 7 | 13 | ⬜ Not Started |
| Phase 8 | 14 | ⬜ Not Started |

## 🔗 Dependencies Flow

```
01 (Setup) → 02 (Types) → 03 (Store) → 04 (Hooks) → 05 (Pipeline)
                                ↓
                         06 (Sync) ←→ 07 (Listeners)
                                ↓
                    08, 09, 10, 11 (Other Side Effects)
                                ↓
                         12 (Advanced Features)
                                ↓
                         13 (Testing) → 14 (Polish)
```

## 💡 Important Notes

- **TypeScript Strict Mode**: All code must pass strict type checking
- **No Premature Optimization**: Optimize only marked critical paths
- **Valtio Integration**: Follow valtio latest documentation for proxy/snapshot usage
- **Minimal Re-renders**: React should only re-render when accessed data changes
- **Atomic Updates**: All changes in a batch must apply together

## 🆘 Need Help?

- Review INPUT.md for original requirements
- Check valtio documentation for proxy patterns
- Refer to CLAUDE.md for TypeScript best practices
