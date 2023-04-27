| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `./target/release/vm run factorial.ron "init=5&n=450000"` | 296.5 ± 9.3 | 287.2 | 312.9 | 1.29 ± 0.05 |
| `python bench/py/fac.py` | 229.8 ± 6.3 | 220.2 | 242.3 | 1.00 |
