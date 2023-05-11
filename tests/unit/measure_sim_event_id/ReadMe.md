This test shows one of the discrepancies between "trace step", and the simulator's "event counter". In `all.csv`:

* between steps 3 & 4, there was no time advancement, but there was event advancement (?!)
* between steps 4 & 5, there was time advancement, but there was no event advancement (?!)
* event id #4 *appears twice*, with two different time values

Inspecting the `trace-summary.json`, we can see how the step #4 corresponds to a perturbation, whereas step #5 to a rule application. Perturbations do not advance the simulation time.
