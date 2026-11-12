# retired/

The four scripts that made test accounts and kept a fake fleet alive. Retired
on 2026-10-01, when every test account on the live server was erased (the
owner's decision, before launch); each one's header says why. The simulated
fleet in use while the launch is delayed is `stack/simulate-driver.py`.

They were written to sit beside `docker-compose.yml` and do not work from
here. To use one on a **development** stack, copy it into `stack/`. Never on
the live server: they create accounts next to real riders.
