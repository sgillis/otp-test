# iohk

## Installation

```
stack build
```

## Execution

```
stack exec --
  iohk --send-for <time-in-ns>
       --wait-for <time-in-ns>
```

Optional flags are `--send-delay` and `--config`

Config file should be in the format

```
<port>,<rng-seed>
```
