# Build & Development

## Environment Setup (One-Time)

```bash
# From project root
ln -sf .envrc.backend .envrc   # For backend work
direnv allow
# This drops you into a Nix develop shell with all dependencies
```

Alternative:
```bash
nix develop .#backend          # Backend shell
nix develop .#frontend         # Frontend shell
```

## Build Commands

```bash
cd Backend
cabal build all                  # Build everything
cabal build <package-name>       # Build specific package (e.g., rider-app)
cabal repl <package-name>        # Interactive REPL for a specific package
```

**Important**: Use `cabal build <target>` for comprehensive checks. `cabal repl` alone doesn't guarantee full compilability. The project uses `-Werror`, so all GHC warnings are compile errors.

## Code Generation (NammaDSL)

The generator (`alchemist` package at `app/alchemist/`) produces Haskell code from YAML specs.

```bash
# Run from Backend/ inside nix shell
, run-generator                  # Only changed specs
, run-generator --all            # All specs
, run-generator --apply-hint     # With HLint auto-fixes
```

### What the Generator Produces

| Input | Output Location | Content |
|-------|----------------|---------|
| `spec/API/*.yaml` | `src-read-only/API/` | Servant API type definitions |
| `spec/Storage/*.yaml` | `src-read-only/Storage/Queries/` | Database query modules |
| `spec/Storage/*.yaml` | `src-read-only/Storage/Beam/` | Beam ORM types |
| `spec/Storage/*.yaml` | `src-read-only/Domain/Types/` | Domain entity types |
| `spec/Storage/*.yaml` | `src-read-only/Storage/CachedQueries/` | Redis-cached queries |
| First run only | `src/Domain/Action/UI/` | Stub files for business logic |

### Critical Rule
**Never edit files in `src-read-only/`** — always modify the source YAML and re-run the generator.

### After Code Generation
Always compile (`cabal build all`) to verify correctness before proceeding.

## Comma Commands (Available in Nix Shell)

| Command | Purpose |
|---------|---------|
| `, run-generator` | Generate code from YAML specs |
| `, run-generator --all` | Regenerate all specs |
| `, run-generator --apply-hint` | Generate with HLint auto-fixes |
| `, run-mobility-stack-dev` | Start all external services (Postgres, Redis, Kafka, etc.) |
| `, ghcid lib/<package-name>` | Fast compile feedback loop |
| `, hpack` | Regenerate .cabal files from package.yaml |
| `, docs` | Run Hoogle documentation server |
| `, kill-svc-ports` | Kill lingering service processes |

## Frontend (PureScript)

```bash
nix develop .#frontend
cd Frontend/ui-customer          # or ui-driver
npm install
npm start                        # Dev server
```

## Linting

```bash
hlint .                          # Haskell linting (uses .hlint.yaml)
```

## Common Compilation Issues

| Issue | Fix |
|-------|-----|
| Unused import error | Remove the import (project uses `-Werror`) |
| Dodgy import warning | Use explicit import list instead of `(..)` |
| `.hs` file deleted but build fails | Run `, hpack` to update `.cabal` file |
| Generated code doesn't compile | Check YAML spec for typos, verify import paths are full module paths |
| `cabal repl` works but `cabal build` fails | `cabal build` checks more strictly; fix all warnings |

## Related Docs

- YAML spec details: `07-namma-dsl.md`
- Haskell conventions: `15-conventions.md`
