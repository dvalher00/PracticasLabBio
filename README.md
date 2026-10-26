# Prácticas en el Laboratorio de Fitopatología

**🌐 Web del proyecto: [dvalher00.github.io/PracticasLabBio](https://dvalher00.github.io/PracticasLabBio/)**

Mis prácticas de análisis de datos de laboratorio (Universidad de Alicante, julio de 2025):
tres actividades en R y Quarto con datos reales de bioensayos.

- **Actividad 1**: germinación de esporas (poder estadístico, ANOVA, Kruskal–Wallis y Dunn).
- **Actividad 2**: capturas de insectos (GLM con distintas distribuciones y diagnóstico con DHARMa).
- **Actividad 3**: eficacia de insecticidas contra *Philaenus spumarius* (LMM, persistencia, PCA y clústeres).

## Datos

Los datos brutos de los ensayos no se publican en este repositorio: pertenecen al Laboratorio de
Fitopatología y, en el caso de la Actividad 3, a la Universidad de Bari, y son confidenciales.
El código, los informes, las figuras y los resultados sí están disponibles. Para volver a ejecutar
un análisis hace falta una copia propia de los datos en la carpeta de la actividad.

## Página web

La [web](https://dvalher00.github.io/PracticasLabBio/) resume todo el trabajo de forma interactiva:
figuras reales de R, comparadores de letras de significación, calculadora de tamaño muestral,
diagnósticos DHARMa por modelo, explorador de eficacia y persistencia con PCA y las diapositivas
de la presentación, con modo claro y oscuro.

Se publica con GitHub Pages desde la carpeta [`docs/`](docs/) de la rama `main`, así que cualquier
cambio que llegue a `main` se despliega solo. El plan de diseño está en [`docs/PLAN.md`](docs/PLAN.md).
También se puede abrir `docs/index.html` directamente en el navegador.
