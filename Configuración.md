# Configuración del entorno de trabajo

## Git

1. Crea una cuenta en [GitHub](https://github.com/).
2. Solicita el *student developer pack* desde [esta dirección](https://education.github.com/pack).
3. Espera entre 2 días y 2 semanas a que te concedan GitHub Pro.

## R y Rstudio

1. Descarga [R](https://cran.r-project.org/bin/windows/base/).
2. Descarga [Rstudio](https://posit.co/download/rstudio-desktop/).
3. Lanza `install.packages("usethis")`.
4. Lanza `usethis`.

## GitHub Desktop

1. Descarga [GitHub Desktop](https://github.com/apps/desktop).

## Rstudio + Git

1. Cambia este código con tus credenciales y lánzalo desde R: `use_git_config(user.name = "Jane", user.email = "jane@example.org")`.
2. Ve a `Global options/Git/SVN` y configura Git.

## Repositorio `aprende_R`

1. Desde `GitHub Desktop` clona el repositorio `RodGal-2020/aprende_R`.
2. Abre `aprende_R.Rproj`.
3. Lanza `renv::install()` y luego escribe `y` para confirmar la instalación de los paquetes.
4. Tu entorno está preparado para todo :).
