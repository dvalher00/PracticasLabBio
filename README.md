# Prácticas en el Laboratorio de Fitopatología

Mis prácticas de análisis de datos de laboratorio (Universidad de Alicante, julio de 2025):
tres actividades en R y Quarto con datos reales de bioensayos.

- **Actividad 1**: germinación de esporas (poder estadístico, ANOVA, Kruskal–Wallis y Dunn).
- **Actividad 2**: capturas de insectos (GLM con distintas distribuciones y diagnóstico con DHARMa).
- **Actividad 3**: eficacia de insecticidas contra *Philaenus spumarius* (LMM, persistencia, PCA y clústeres).

## Página web

La carpeta [`docs/`](docs/) contiene una web estática que resume todo el trabajo
(HTML, CSS y JS sin dependencias, con las figuras reales, modo claro/oscuro y las diapositivas).
El plan de diseño está en [`docs/PLAN.md`](docs/PLAN.md).

Para publicarla con GitHub Pages: *Settings → Pages → Build and deployment → Source: Deploy from a branch*,
rama `main` y carpeta `/docs`. También se puede abrir `docs/index.html` directamente en el navegador.
