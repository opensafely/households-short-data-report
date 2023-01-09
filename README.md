# households-short-data-report

This is the code and configuration for a now retired short report on households in opensafely. The purpose was to check the feasibility of linking individuals within the same household for analysis, based on the available household identifiers. Notes on the process and conclusions can be found here(https://docs.google.com/document/d/1w_tETnDhf3mNgbFy0WTfn6TV5qoOAmzZFR3-s8HA2HA/edit?usp=sharing), here(https://docs.google.com/document/d/1zHiJULgnrbOUTXU67Lw45Yv22hC5e6Knx5FQWlBgMPc/edit?usp=sharing) and here(https://docs.google.com/document/d/1szBdfUN8UlNbXK9gX4SSLGOCUqJZqI9paGRd0u_8zKo/edit?usp=sharing).

You can run this project via [Gitpod](https://gitpod.io) in a web browser by clicking on this badge: [![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-908a85?logo=gitpod)](https://gitpod.io/#https://github.com/opensafely/households-short-data-report)

* Raw model outputs, including charts, crosstabs, etc, are in `released_outputs/`
* If you are interested in how we defined our variables, take a look at the [study definition](analysis/study_definition.py); this is written in `python`, but non-programmers should be able to understand what is going on there
* If you are interested in how we defined our code lists, look in the [codelists folder](./codelists/).
* Developers and epidemiologists interested in the framework should review [the OpenSAFELY documentation](https://docs.opensafely.org)

# About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic
health records research in the NHS, with a focus on public accountability and
research quality.

Read more at [OpenSAFELY.org](https://opensafely.org).

# Licences
As standard, research projects have a MIT license. 
