# diff-eq-simulations


## About

A shiny app which simulates a pendulum by numerically iterating through the governing differential equation.  The purpose was to further understand differential equations by replicating the animations created by the YouTube channel, [3Blue1Brown](https://www.youtube.com/channel/UCYO_jab_esuFRV4b17AJtAw).  Being a shiny app, it allows users to adjust the parameters shown in the [video](https://www.youtube.com/watch?v=p_di4Zn4wz4&t=945s) and see how it changes the vector field and the resulting pendulum simulation.

Below is an image of the app.
![](https://raw.githubusercontent.com/adamBirenbaum/diff-eq-simulations/README_files/readme_app.png)


![](https://raw.githubusercontent.com/adamBirenbaum/diff-eq-simulations/README_files/traj.gif)
![](https://raw.githubusercontent.com/adamBirenbaum/diff-eq-simulations/README_files/vec_field.gif)

## Setup

Requires latest CRAN version of `gganimate`.

Should only require one customization:   The `path_to_folder` variable at the top of `ui.R` should be changed to an existing directory on your machine.  This directory will temporarily hold the gifs.

