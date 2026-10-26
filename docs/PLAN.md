# Plan de la web · Prácticas en el Laboratorio de Fitopatología

Este documento es el *prompt* de diseño que se planificó antes de construir la página.
Sirve como brief: qué se cuenta, para quién, con qué tema visual y con qué estructura.

## 1. Brief (prompt de diseño)

> Construye una web estática de una sola página (scroll largo con navegación fija) que
> presente las prácticas externas de análisis de datos realizadas por David Valcárcel
> Herrera en el Laboratorio de Fitopatología (Dpto. de Ciencias del Mar y Biología
> Aplicada, Universidad de Alicante) en julio de 2025. La web debe explicar las tres
> actividades (germinación de esporas, conteo de insectos y eficacia de insecticidas
> contra *Philaenus spumarius*), el flujo estadístico seguido en cada una (ANOVA,
> transformaciones, Kruskal–Wallis/Dunn, GLM, LMM/GLMM, PCA y clústeres) y las
> conclusiones, reutilizando las figuras reales generadas en R. Público: tutores
> académicos, investigadores del laboratorio y cualquier persona que llegue desde el
> repositorio de GitHub. Idioma: español, con los términos técnicos originales en inglés
> cuando proceda (la Actividad 3 se entregó en inglés).

## 2. Tema visual elegido: "cuaderno de laboratorio botánico"

| Decisión | Elección | Motivo |
|---|---|---|
| Identidad | Cuaderno de laboratorio de fitopatología | Es literalmente dónde y cómo se hizo el trabajo (Quarto = libreta de laboratorio digital). |
| Paleta | Verde hoja profundo (primario), verde musgo (superficies), ámbar "espuma de espumadora" (acento), papel verdoso frío (fondo), tinta verde-negra (texto) | Verde por la fitopatología; el ámbar recuerda a las masas de espuma de *P. spumarius* y rompe el monocromo. |
| Modo oscuro | Sí, con tokens CSS y `prefers-color-scheme` + `data-theme` | La web se puede leer en ambos temas sin perder contraste. |
| Tipografía | Display: *Fraunces* (serif con carácter). Cuerpo: *Source Sans 3*. Datos/código: *IBM Plex Mono* | Contraste serif/sans propio de una publicación científica, mono para fórmulas, variables y letras de grupos. |
| Dispositivo gráfico | Reglas finas, etiquetas en mayúsculas con tracking ("ACTIVIDAD 01"), letras de significación (a, b, c) como motivo | Las letras de grupos de Tukey/Dunn son el detalle que solo este trabajo tendría. |
| Figuras | PNG reales del repositorio, con pie de figura y lightbox | No se inventan gráficos: se reutilizan los generados en R. |

## 3. Estructura de la página

1. **Hero**: título, ficha del alumno/tutor/lugar/periodo, stack (R · Quarto · dplyr · ggplot2).
2. **Resumen en cifras**: 3 actividades, 2 campañas (2017/2018), 9 tratamientos × 2 tiempos, 5 familias de modelos.
3. **Contexto**: dónde, con quién, objetivo de las prácticas, cómo era un día de trabajo.
4. **Metodología común**: flujo datos → diagnóstico → transformación → modelo → post-hoc → visualización.
5. **Actividad 1** · Germinación de esporas: poder estadístico, ANOVA → Kruskal–Wallis + Dunn, boxplots 8 h / 24 h, conclusiones (dosis óptima 20 µg/ml, el DMSO no explica el efecto).
6. **Actividad 2** · Capturas de insectos: comparación de GLM (gaussiano, Poisson, binomial negativa, beta, gamma) con DHARMa, boxplot con letras, conclusión (A+B).
7. **Actividad 3** · Eficacia de ISOCLAST (sulfoxaflor) contra *Philaenus spumarius*: tres preguntas de investigación con pestañas 2017/2018, partición de varianza (tratamiento vs. parcela), curvas de persistencia (AUC, codo, Δ_elbow), PCA + clústeres, diagrama resumen, tablas de tratamientos.
8. **Conceptos aprendidos**: glosario en tarjetas (poder estadístico, Box–Cox, factor anidado vs. cruzado, LMM/GLMM, LMM ponderado, DHARMa, BLUP, punto de codo).
9. **Presentación**: carrusel con las 24 diapositivas entregadas a la Universidad de Bari.
10. **Valoración personal y descargas**: memoria PDF, presentación PDF, informes Quarto de cada actividad y repositorio.

## 4. Requisitos técnicos

- HTML + CSS + JS sin frameworks ni build. Fuentes desde Google Fonts con fallback.
- Responsive (≥ 360 px), sin scroll horizontal, navegación con *scroll-spy*, botón de tema.
- Accesible: contraste AA, foco visible, `alt` en todas las figuras, `prefers-reduced-motion`.
- Publicable en GitHub Pages desde la carpeta `docs/` sin configuración extra.
- Datos confidenciales: no se publican las tablas crudas de la Actividad 3, solo figuras y conclusiones (igual que en el repositorio).

## 5. Tareas

- [x] Leer memoria, presentación y cuadernos Quarto para extraer el contenido.
- [x] Seleccionar las figuras representativas y copiarlas a `docs/assets/img`.
- [x] Escribir `docs/index.html`, `docs/css/style.css` y `docs/js/main.js`.
- [x] Comprobar el renderizado en escritorio y móvil, en claro y oscuro.
- [x] Documentar cómo activar GitHub Pages en el README.
