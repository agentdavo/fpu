# Repository Guidelines

This repo contains a simplified SpinalHDL model of the T9000 FPU. The design is
not cycle accurate but mirrors the original stack based architecture.  The list
of operation codes can be found in `hw/spinal/fpu/fpu_instructions.txt` and
should be consulted when extending the implementation.

## Build
- Use `sbt compile` to ensure the sources build.
- Run the simulation with `sbt "runMain fpu.FpuSimPipeline"` which exercises a
  few floating point operations.

## Contribution Notes
- The design is intentionally lightweight and incomplete.
- Keep the five stage stack pipeline (n0..n4) when adding features and try to
  match the documented cycle latencies.
- Improving IEEE-754 compliance and expanding the instruction set is welcomed
  but not mandatory for every contribution.
