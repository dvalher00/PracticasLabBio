(function () {
  'use strict';

  /* =====================================================================
     Utilidades
     ===================================================================== */
  function $(sel, ctx) { return (ctx || document).querySelector(sel); }
  function $$(sel, ctx) { return Array.prototype.slice.call((ctx || document).querySelectorAll(sel)); }
  function fmt(x, d) {
    var s = Math.abs(x).toFixed(d === undefined ? 1 : d).replace('.', ',');
    return (x < 0 && +s.replace(',', '.') !== 0 ? '−' : '') + s;
  }
  function joinEs(list) {
    if (list.length <= 1) return list.join('');
    return list.slice(0, -1).join(', ') + ' y ' + list[list.length - 1];
  }
  function esc(s) { return String(s).replace(/[&<>"]/g, function (c) { return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]; }); }
  function onResize(el, fn) {
    if (!el) return;
    if ('ResizeObserver' in window) {
      var last = 0;
      new ResizeObserver(function () { var w = el.clientWidth; if (w && w !== last) { last = w; fn(); } }).observe(el);
    } else { window.addEventListener('resize', fn); }
    fn();
  }
  /* Grupo de botones segmentados: marca aria-pressed y avisa del valor elegido. */
  function seg(group, attr, cb) {
    if (!group) return;
    group.addEventListener('click', function (e) {
      var b = e.target.closest('button[' + attr + ']');
      if (!b) return;
      $$('button[' + attr + ']', group).forEach(function (x) { x.setAttribute('aria-pressed', x === b ? 'true' : 'false'); });
      cb(b.getAttribute(attr));
    });
  }

  /* =====================================================================
     Tema claro / oscuro
     ===================================================================== */
  var root = document.documentElement;
  function readTheme() { try { return localStorage.getItem('theme'); } catch (e) { return null; } }
  function saveTheme(v) { try { localStorage.setItem('theme', v); } catch (e) { /* sin almacenamiento */ } }
  var saved = readTheme();
  if (saved === 'dark' || saved === 'light') root.setAttribute('data-theme', saved);
  var toggle = $('#theme-toggle');
  if (toggle) toggle.addEventListener('click', function () {
    var current = root.getAttribute('data-theme');
    var systemDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
    var next = (current ? current === 'dark' : systemDark) ? 'light' : 'dark';
    root.setAttribute('data-theme', next);
    saveTheme(next);
  });

  /* =====================================================================
     Menú móvil, scroll-spy y barra de progreso
     ===================================================================== */
  var navToggle = $('.nav__toggle'), navList = $('#nav-list');
  if (navToggle && navList) {
    navToggle.addEventListener('click', function () {
      var open = navList.classList.toggle('is-open');
      navToggle.setAttribute('aria-expanded', open ? 'true' : 'false');
    });
    navList.addEventListener('click', function (e) {
      if (e.target.tagName === 'A') { navList.classList.remove('is-open'); navToggle.setAttribute('aria-expanded', 'false'); }
    });
  }
  var links = $$('.nav__list a[href^="#"]');
  var sections = links.map(function (a) { return $(a.getAttribute('href')); }).filter(Boolean);
  if ('IntersectionObserver' in window && sections.length) {
    var io = new IntersectionObserver(function (entries) {
      entries.forEach(function (en) {
        if (en.isIntersecting) links.forEach(function (a) { a.classList.toggle('is-active', a.getAttribute('href') === '#' + en.target.id); });
      });
    }, { rootMargin: '-40% 0px -55% 0px', threshold: 0 });
    sections.forEach(function (s) { io.observe(s); });
  }
  var bar = $('#progress-bar'), ticking = false;
  function progress() {
    var h = document.documentElement.scrollHeight - window.innerHeight;
    if (bar) bar.style.transform = 'scaleX(' + (h > 0 ? Math.min(1, window.scrollY / h) : 0) + ')';
    ticking = false;
  }
  window.addEventListener('scroll', function () { if (!ticking) { ticking = true; requestAnimationFrame(progress); } }, { passive: true });
  progress();

  /* =====================================================================
     Pestañas
     ===================================================================== */
  $$('[data-tabs]').forEach(function (tabs) {
    var btns = $$('[role="tab"]', tabs), panels = $$('[role="tabpanel"]', tabs);
    function select(i) {
      btns.forEach(function (b, j) { b.setAttribute('aria-selected', i === j ? 'true' : 'false'); b.tabIndex = i === j ? 0 : -1; });
      panels.forEach(function (p, j) { p.hidden = i !== j; });
    }
    btns.forEach(function (b, i) {
      b.addEventListener('click', function () { select(i); });
      b.addEventListener('keydown', function (e) {
        var n = null;
        if (e.key === 'ArrowRight') n = (i + 1) % btns.length;
        if (e.key === 'ArrowLeft') n = (i - 1 + btns.length) % btns.length;
        if (n !== null) { e.preventDefault(); select(n); btns[n].focus(); }
      });
    });
  });

  /* =====================================================================
     Comparador de letras de significación (datos de las figuras en R)
     ===================================================================== */
  var CLD = {
    a1_8h: [['Control', 'a'], ['1 µg/ml', 'a'], ['2,5 µg/ml', 'ab'], ['5 µg/ml', 'abc'], ['7,5 µg/ml', 'abc'], ['10 µg/ml', 'bc'], ['20 µg/ml', 'c'], ['30 µg/ml', 'c'], ['DMSO 0,35 %', 'a']],
    a1_24h: [['Control', 'b'], ['1 µg/ml', 'ab'], ['2,5 µg/ml', 'ac'], ['5 µg/ml', 'ab'], ['7,5 µg/ml', 'abc'], ['10 µg/ml', 'ac'], ['20 µg/ml', 'c'], ['30 µg/ml', 'c'], ['DMSO 0,35 %', 'ab']],
    a2: [['Control', 'b'], ['A', 'ab'], ['B', 'b'], ['A+B', 'a']],
    a3_2017: [['Sin tratar', 'a'], ['GF-2626 200 fol.', 'b'], ['GF-2626 400 fol.', 'b'], ['GF-2626 400 iny.', 'b'], ['CONFIDOR fol.', 'd'], ['CONFIDOR iny.', 'a'], ['KARATE fol.', 'c']],
    a3_2018: [['Sin tratar', 'a'], ['CONFIDOR fol.', 'b'], ['GF-2626 400 fol.', 'a'], ['GF-2626 400 iny.', 'a'], ['EPIK fol.', 'b'], ['EPIK iny.', 'b']]
  };
  function shares(a, b) { for (var i = 0; i < a.length; i++) if (b.indexOf(a[i]) >= 0) return true; return false; }
  function renderCld(box) {
    var data = CLD[box.getAttribute('data-set')] || [];
    var chips = $('.cld__chips', box), out = $('.cld__result', box);
    chips.innerHTML = data.map(function (d, i) {
      return '<button type="button" class="cld__chip" data-i="' + i + '" aria-pressed="false">' + esc(d[0]) + ' <span class="mono">' + d[1] + '</span></button>';
    }).join('');
    out.textContent = 'Dos tratamientos que comparten alguna letra no difieren de forma significativa.';
  }
  $$('.cld').forEach(function (box) {
    renderCld(box);
    $('.cld__chips', box).addEventListener('click', function (e) {
      var b = e.target.closest('.cld__chip');
      if (!b) return;
      var data = CLD[box.getAttribute('data-set')], i = +b.getAttribute('data-i'), me = data[i];
      var same = [], diff = [];
      $$('.cld__chip', box).forEach(function (c, j) {
        c.classList.remove('is-same', 'is-diff', 'is-sel');
        c.setAttribute('aria-pressed', j === i ? 'true' : 'false');
        if (j === i) { c.classList.add('is-sel'); return; }
        if (shares(me[1], data[j][1])) { c.classList.add('is-same'); same.push(data[j][0]); }
        else { c.classList.add('is-diff'); diff.push(data[j][0]); }
      });
      var html = '<strong>' + esc(me[0]) + '</strong> <span class="mono">(' + me[1] + ')</span> ';
      html += same.length ? 'no difiere de ' + esc(joinEs(same)) + '. ' : 'no comparte grupo con ningún otro tratamiento. ';
      html += diff.length ? 'Difiere de forma significativa de ' + esc(joinEs(diff)) + '.' : 'No difiere de forma significativa de ninguno.';
      $('.cld__result', box).innerHTML = html;
    });
  });

  /* =====================================================================
     Actividad 1 · visor de figuras 8 h / 24 h, boxplot / violín
     ===================================================================== */
  var A1 = {
    '8h_boxplot': ['assets/img/a1_boxplot_8h.png', 1897, 1036, 'Boxplot del porcentaje de germinación por tratamiento a 8 horas con letras de grupos.'],
    '8h_violin': ['assets/img/a1_violin_8h.png', 1840, 879, 'Gráfico de violín del porcentaje de germinación por tratamiento a 8 horas con letras de grupos.'],
    '24h_boxplot': ['assets/img/a1_boxplot_24h.png', 1912, 1035, 'Boxplot del porcentaje de germinación por tratamiento a 24 horas con letras de grupos.'],
    '24h_violin': ['assets/img/a1_violin_24h.png', 1798, 759, 'Gráfico de violín del porcentaje de germinación por tratamiento a 24 horas con letras de grupos.']
  };
  var A1CAP = {
    '8h': '<strong>8 h.</strong> Kruskal–Wallis χ² = 70,2 (8 gl, p &lt; 0,001). A partir de 10 µg/ml la germinación cae de forma significativa respecto al control.',
    '24h': '<strong>24 h.</strong> Kruskal–Wallis χ² = 67,0 (8 gl, p &lt; 0,001). Mismo patrón, con porcentajes de germinación más altos en general.'
  };
  var a1 = { time: '8h', plot: 'boxplot' };
  function a1Update() {
    var d = A1[a1.time + '_' + a1.plot], img = $('#a1-img');
    if (!img) return;
    img.src = d[0]; img.width = d[1]; img.height = d[2]; img.alt = d[3];
    $('#a1-link').href = d[0];
    $('#a1-cap').innerHTML = A1CAP[a1.time];
    var box = $('#a1-cld');
    box.setAttribute('data-set', 'a1_' + a1.time);
    renderCld(box);
  }
  $$('#a1-viewer .seg').forEach(function (g) {
    seg(g, 'data-time', function (v) { a1.time = v; a1Update(); });
    seg(g, 'data-plot', function (v) { a1.plot = v; a1Update(); });
  });

  /* =====================================================================
     Actividad 1 · calculadora de tamaño muestral (como pwr.anova.test)
     ===================================================================== */
  function lgamma(x) {
    var c = [76.18009172947146, -86.50532032941677, 24.01409824083091, -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5];
    var y = x, t = x + 5.5; t -= (x + 0.5) * Math.log(t);
    var s = 1.000000000190015;
    for (var j = 0; j < 6; j++) { y += 1; s += c[j] / y; }
    return -t + Math.log(2.5066282746310005 * s / x);
  }
  function betacf(a, b, x) {
    var MAXIT = 300, EPS = 3e-14, FPMIN = 1e-300, qab = a + b, qap = a + 1, qam = a - 1, c = 1, d = 1 - qab * x / qap;
    if (Math.abs(d) < FPMIN) d = FPMIN;
    d = 1 / d; var h = d;
    for (var m = 1; m <= MAXIT; m++) {
      var m2 = 2 * m, aa = m * (b - m) * x / ((qam + m2) * (a + m2));
      d = 1 + aa * d; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = 1 + aa / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d; h *= d * c;
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
      d = 1 + aa * d; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = 1 + aa / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d; var del = d * c; h *= del;
      if (Math.abs(del - 1) < EPS) break;
    }
    return h;
  }
  function ibeta(x, a, b) {
    if (x <= 0) return 0;
    if (x >= 1) return 1;
    var bt = Math.exp(lgamma(a + b) - lgamma(a) - lgamma(b) + a * Math.log(x) + b * Math.log(1 - x));
    return x < (a + 1) / (a + b + 2) ? bt * betacf(a, b, x) / a : 1 - bt * betacf(b, a, 1 - x) / b;
  }
  function pf(x, d1, d2) { return x <= 0 ? 0 : ibeta(d1 * x / (d1 * x + d2), d1 / 2, d2 / 2); }
  function qf(p, d1, d2) {
    var lo = 0, hi = 1;
    while (pf(hi, d1, d2) < p) hi *= 2;
    for (var i = 0; i < 70; i++) { var mid = (lo + hi) / 2; if (pf(mid, d1, d2) < p) lo = mid; else hi = mid; }
    return (lo + hi) / 2;
  }
  /* F no central: mezcla de Poisson de betas incompletas, sumada alrededor de la moda. */
  function pncf(x, d1, d2, lam) {
    if (x <= 0) return 0;
    var y = d1 * x / (d1 * x + d2), h = lam / 2, w = 12 * Math.sqrt(h + 1) + 30;
    var j0 = Math.max(0, Math.floor(h - w)), j1 = Math.ceil(h + w), s = 0;
    for (var j = j0; j <= j1; j++) {
      var lw = -h + (j ? j * Math.log(h) : 0) - lgamma(j + 1);
      s += Math.exp(lw) * ibeta(y, d1 / 2 + j, d2 / 2);
    }
    return Math.min(1, s);
  }
  function anovaPower(k, n, f, alpha) {
    var d1 = k - 1, d2 = (n - 1) * k;
    return 1 - pncf(qf(1 - alpha, d1, d2), d1, d2, k * n * f * f);
  }
  function anovaN(k, f, power, alpha) {
    var lo = 2 + 1e-10, hi = 4;
    while (anovaPower(k, hi, f, alpha) < power) { lo = hi; hi *= 2; if (hi > 1e5) return Infinity; }
    for (var i = 0; i < 50 && hi - lo > 1e-4; i++) {
      var mid = (lo + hi) / 2;
      if (anovaPower(k, mid, f, alpha) < power) lo = mid; else hi = mid;
    }
    return (lo + hi) / 2;
  }

  var calc = $('#power-calc');
  if (calc) {
    var inF = $('#pc-f'), inP = $('#pc-power'), inK = $('#pc-k'), chart = $('#pc-chart'), resetBtn = $('#pc-reset');
    var ACT = { f: 0.45, p: 0.75, k: 9 }, pending = false, lastN = 8.354;
    function powerChart(k, f, p, n) {
      var W = Math.max(260, chart.clientWidth), H = Math.round(Math.min(220, W * 0.5));
      var m = { l: 40, r: 14, t: 12, b: 34 }, pw = W - m.l - m.r, ph = H - m.t - m.b;
      var nMax = Math.max(10, Math.ceil(n * 2)), nMin = 2, pts = [], steps = 48;
      for (var i = 0; i <= steps; i++) {
        var nn = nMin + (nMax - nMin) * i / steps;
        pts.push([nn, anovaPower(k, Math.max(nn, 2.001), f, 0.05)]);
      }
      function X(v) { return m.l + (v - nMin) / (nMax - nMin) * pw; }
      function Y(v) { return m.t + (1 - v) * ph; }
      var tickStep = nMax <= 20 ? 2 : nMax <= 50 ? 5 : nMax <= 120 ? 10 : nMax <= 300 ? 25 : 100;
      var g = '';
      [0, 0.25, 0.5, 0.75, 1].forEach(function (v) {
        g += '<line class="grid" x1="' + m.l + '" x2="' + (W - m.r) + '" y1="' + Y(v) + '" y2="' + Y(v) + '"/>';
        g += '<text class="tick" x="' + (m.l - 6) + '" y="' + (Y(v) + 4) + '" text-anchor="end">' + fmt(v, 2) + '</text>';
      });
      for (var t = Math.ceil(nMin / tickStep) * tickStep; t <= nMax; t += tickStep) {
        g += '<text class="tick" x="' + X(t) + '" y="' + (H - m.b + 16) + '" text-anchor="middle">' + t + '</text>';
      }
      var path = pts.map(function (q, i) { return (i ? 'L' : 'M') + X(q[0]).toFixed(1) + ' ' + Y(q[1]).toFixed(1); }).join(' ');
      var area = path + ' L' + X(nMax).toFixed(1) + ' ' + Y(0) + ' L' + X(nMin).toFixed(1) + ' ' + Y(0) + ' Z';
      var nx = X(Math.min(n, nMax)), py = Y(p);
      chart.innerHTML = '<svg viewBox="0 0 ' + W + ' ' + H + '" width="' + W + '" height="' + H + '">' + g +
        '<path class="area" d="' + area + '"/><path class="curve" d="' + path + '"/>' +
        '<line class="target" x1="' + m.l + '" x2="' + (W - m.r) + '" y1="' + py + '" y2="' + py + '"/>' +
        '<line class="marker" x1="' + nx + '" x2="' + nx + '" y1="' + py + '" y2="' + Y(0) + '"/>' +
        '<circle class="dot" cx="' + nx + '" cy="' + py + '" r="5"/>' +
        '<text class="axis" x="' + (m.l + pw / 2) + '" y="' + (H - 4) + '" text-anchor="middle">réplicas por grupo (n)</text>' +
        '<text class="axis" x="' + (m.l + 4) + '" y="' + (py - 6) + '">poder objetivo</text></svg>';
    }
    function update() {
      pending = false;
      var f = +inF.value, p = +inP.value, k = +inK.value;
      $('#pc-f-out').textContent = fmt(f, 2);
      $('#pc-power-out').textContent = fmt(p, 2);
      $('#pc-k-out').textContent = k;
      var n = anovaN(k, f, p, 0.05);
      lastN = n;
      var nc = Math.ceil(n);
      $('#pc-n').textContent = isFinite(nc) ? nc : '—';
      $('#pc-N').textContent = isFinite(nc) ? nc * k : '—';
      $('#pc-nexact').textContent = 'n = ' + fmt(n, 2);
      $('#pc-ktimes').textContent = k + ' grupos × ' + nc;
      var isAct = Math.abs(f - ACT.f) < 1e-9 && Math.abs(p - ACT.p) < 1e-9 && k === ACT.k;
      $('#pc-note').textContent = isAct
        ? 'Valores de la actividad: f = 0,45, poder 0,75 y 9 grupos.'
        : 'Con f = ' + fmt(f, 2) + ', poder ' + fmt(p, 2) + ' y ' + k + ' grupos hacen falta ' + nc + ' réplicas por grupo. En la actividad bastaban 9.';
      resetBtn.hidden = isAct;
      $$('.calc__presets button', calc).forEach(function (b) { b.setAttribute('aria-pressed', Math.abs(+b.getAttribute('data-f') - f) < 1e-9 ? 'true' : 'false'); });
      powerChart(k, f, p, n);
    }
    function schedule() { if (!pending) { pending = true; requestAnimationFrame(update); } }
    [inF, inP, inK].forEach(function (el) { el.addEventListener('input', schedule); });
    $$('.calc__presets button', calc).forEach(function (b) { b.addEventListener('click', function () { inF.value = b.getAttribute('data-f'); schedule(); }); });
    resetBtn.addEventListener('click', function () { inF.value = ACT.f; inP.value = ACT.p; inK.value = ACT.k; schedule(); });
    onResize(chart, function () { powerChart(+inK.value, +inF.value, +inP.value, lastN); });
    update();
  }

  /* =====================================================================
     Actividad 2 · diagnóstico DHARMa por modelo
     ===================================================================== */
  var DHARMA = {
    gaussian: { name: 'gaussiana', img: 'assets/img/a2_dharma_gaussian.png', alt: 'Diagnóstico DHARMa del modelo gaussiano: el QQ plot se curva y el gráfico de residuos frente a predicción marca desviaciones de cuantiles.',
      checks: [['ok', 'Uniformidad (KS)', 'p = 0,105'], ['ok', 'Dispersión', 'p = 0,824'], ['ok', 'Valores atípicos', 'p = 0,274'], ['bad', 'Residuo frente a predicción', 'desviaciones']] },
    poisson: { name: 'Poisson', img: 'assets/img/a2_dharma_poisson.png', alt: 'Diagnóstico DHARMa del modelo de Poisson: el QQ plot se aleja mucho de la diagonal y las tres pruebas son significativas.',
      checks: [['bad', 'Uniformidad (KS)', 'p ≈ 0'], ['bad', 'Dispersión', 'p ≈ 0'], ['bad', 'Valores atípicos', 'p ≈ 0'], ['bad', 'Residuo frente a predicción', 'desviaciones']] },
    negbin: { name: 'binomial negativa', img: 'assets/img/a2_dharma_negbin.png', alt: 'Diagnóstico DHARMa del modelo binomial negativo: QQ plot sobre la diagonal y residuos sin desviaciones.',
      checks: [['ok', 'Uniformidad (KS)', 'p = 0,951'], ['ok', 'Dispersión', 'p = 0,528'], ['ok', 'Valores atípicos', 'p = 1'], ['ok', 'Residuo frente a predicción', 'sin problemas']] }
  };
  var glmTable = $('#glm-table');
  if (glmTable) glmTable.addEventListener('click', function (e) {
    var b = e.target.closest('.pick');
    if (!b) return;
    var row = b.closest('tr'), d = DHARMA[row.getAttribute('data-model')];
    $$('tr[data-model]', glmTable).forEach(function (r) {
      var on = r === row;
      r.classList.toggle('is-picked', on);
      $('.pick', r).setAttribute('aria-pressed', on ? 'true' : 'false');
    });
    $('#dharma-title').textContent = 'Diagnóstico DHARMa · ' + d.name;
    var img = $('#dharma-img'); img.src = d.img; img.alt = d.alt;
    $('#dharma-link').href = d.img;
    $('#dharma-checks').innerHTML = d.checks.map(function (c) {
      return '<li class="' + c[0] + '"><span>' + c[1] + '</span><span class="mono">' + c[2] + '</span></li>';
    }).join('');
  });

  /* =====================================================================
     Actividad 3 · curvas de persistencia por campaña
     ===================================================================== */
  var CURVES = {
    pooled: ['assets/img/a3_curves_pooled.png', 1386, 787, 'Curvas de mortalidad media por periodo para los nueve tratamientos agrupando los dos años.',
      'Mortalidad media por periodo, agrupando 2017 y 2018. Las líneas verticales marcan los puntos de codo de algunos tratamientos.'],
    '2017': ['assets/img/a3_curves_2017.png', 1405, 837, 'Curvas de mortalidad media por periodo dentro de 2017 para los siete tratamientos.',
      'Mortalidad media por periodo dentro de 2017. La leyenda original marca como inyección GF-2626 a 200 ml/ha y KARATE ZEON, pero según la tabla de tratamientos ambos se aplicaron por vía foliar.'],
    '2018': ['assets/img/a3_curves_2018.png', 1384, 793, 'Curvas de mortalidad media por periodo dentro de 2018 para los seis tratamientos.',
      'Mortalidad media por periodo dentro de 2018, con ocho periodos de muestreo. EPIK foliar alcanza el máximo pero cae con fuerza al final.']
  };
  seg($('#curves-viewer .seg'), 'data-curves', function (v) {
    var d = CURVES[v], img = $('#curves-img');
    img.src = d[0]; img.width = d[1]; img.height = d[2]; img.alt = d[3];
    $('#curves-link').href = d[0];
    $('#curves-cap').textContent = d[4];
  });

  /* =====================================================================
     Actividad 3 · explorador de persistencia
     Medias ± sd y letras: tabla de la diapositiva 12 de la presentación.
     PCA: recalculado con esas medias (8 tratamientos, variables estandarizadas);
     reproduce el 82,5 % / 17,3 % y las cargas del informe.
     ===================================================================== */
  var TRT = [
    { id: 'UNTREATED', name: 'Sin tratar', short: 'Sin tratar', dose: 'Control', method: '—', cluster: 0, verdict: '',
      auc: [72.8, 37.8, 'a'], fin: [20.5, 9.3, 'a'], elb: [17.0, 12.0, 'a'], dlt: [3.5, 7.4, 'a'], pca: null },
    { id: 'CONFIDOR_750_INJCTREE', name: 'CONFIDOR 200 OD', short: 'CONFIDOR iny.', dose: '750 ml/ha', method: 'Inyección', cluster: 2, verdict: 'worst',
      auc: [120.8, 59.0, 'ab'], fin: [24.7, 11.9, 'a'], elb: [34.1, 19.0, 'ab'], dlt: [-9.3, 17.0, 'ab'], pca: [1.889, 1.422] },
    { id: 'SULFOXAFLOR_400_INJCTREE', name: 'Sulfoxaflor (GF-2626)', short: 'SULFOX 400 iny.', dose: '400 ml/ha', method: 'Inyección', cluster: 2, verdict: '',
      auc: [158.8, 125.5, 'b'], fin: [40.2, 26.8, 'b'], elb: [34.0, 29.5, 'b'], dlt: [6.2, 23.2, 'abc'], pca: [1.861, -0.378] },
    { id: 'SULFOXAFLOR_400_FOLIAR', name: 'Sulfoxaflor (GF-2626)', short: 'SULFOX 400 fol.', dose: '400 ml/ha', method: 'Foliar', cluster: 2, verdict: '',
      auc: [165.1, 76.6, 'b'], fin: [39.9, 19.4, 'b'], elb: [39.3, 23.1, 'b'], dlt: [0.5, 23.2, 'abc'], pca: [1.488, 0.095] },
    { id: 'SULFOXAFLOR_200_FOLIAR', name: 'Sulfoxaflor (GF-2626)', short: 'SULFOX 200 fol.', dose: '200 ml/ha', method: 'Foliar', cluster: 2, verdict: '',
      auc: [182.6, 70.1, 'b'], fin: [52.1, 23.6, 'bc'], elb: [45.3, 21.6, 'bc'], dlt: [6.8, 30.9, 'bc'], pca: [1.153, -0.823] },
    { id: 'KARATE_250_FOLIAR', name: 'KARATE ZEON 10 CS', short: 'KARATE fol.', dose: '250 ml/ha', method: 'Foliar', cluster: 1, verdict: 'best',
      auc: [280.0, 78.5, 'c'], fin: [66.8, 21.6, 'cd'], elb: [64.2, 17.7, 'cd'], dlt: [2.6, 23.5, 'abc'], pca: [-0.377, -1.031] },
    { id: 'EPIK_1500_INJCTREE', name: 'EPIK', short: 'EPIK iny.', dose: '1500 g/ha', method: 'Inyección', cluster: 1, verdict: 'good',
      auc: [330.8, 93.2, 'c'], fin: [64.3, 21.5, 'cd'], elb: [79.3, 18.2, 'de'], dlt: [-15.0, 16.6, 'c'], pca: [-1.612, 0.450] },
    { id: 'CONFIDOR_750_FOLIAR', name: 'CONFIDOR 200 OD', short: 'CONFIDOR fol.', dose: '750 ml/ha', method: 'Foliar', cluster: 1, verdict: 'best',
      auc: [379.2, 108.2, 'c'], fin: [74.5, 17.3, 'd'], elb: [83.0, 25.0, 'de'], dlt: [-8.5, 16.8, 'c'], pca: [-1.984, -0.452] },
    { id: 'EPIK_1500_FOLIAR', name: 'EPIK', short: 'EPIK fol.', dose: '1500 g/ha', method: 'Foliar', cluster: 1, verdict: 'good',
      auc: [391.7, 44.2, 'c'], fin: [67.6, 13.7, 'd'], elb: [87.7, 10.8, 'e'], dlt: [-20.2, 17.8, 'bc'], pca: [-2.419, 0.717] }
  ];
  var VARS = {
    auc: { label: 'AUC', axis: 'AUC (área bajo la curva)' },
    fin: { label: 'm final', axis: 'Mortalidad final (%)' },
    elb: { label: 'm codo', axis: 'Mortalidad en el codo (%)' },
    dlt: { label: 'Δ codo', axis: 'Δ codo = m final − m codo (puntos)' }
  };
  var VERDICT = { best: ['Mejor', 'badge--best'], good: ['Bueno', 'badge--good'], worst: ['Peor', 'badge--worst'] };
  var byId = {}; TRT.forEach(function (t) { byId[t.id] = t; });

  var ex = $('#explorer');
  if (ex) {
    var exChart = $('#ex-chart'), exDetail = $('#ex-detail'), exCap = $('#ex-caption');
    var selX = $('#ex-x'), selY = $('#ex-y'), chkSd = $('#ex-sd'), tbody = $('#ex-table tbody');
    var state = { view: 'pca', x: 'auc', y: 'dlt', sd: false, sel: 'CONFIDOR_750_FOLIAR', hover: null, sort: 'auc', dir: -1 };

    Object.keys(VARS).forEach(function (k) {
      selX.insertAdjacentHTML('beforeend', '<option value="' + k + '">' + VARS[k].axis + '</option>');
      selY.insertAdjacentHTML('beforeend', '<option value="' + k + '">' + VARS[k].axis + '</option>');
    });
    selX.value = state.x; selY.value = state.y;

    function niceTicks(min, max, count) {
      var span = max - min, step = Math.pow(10, Math.floor(Math.log10(span / count))), err = span / count / step;
      if (err >= 7.5) step *= 10; else if (err >= 3.5) step *= 5; else if (err >= 1.5) step *= 2;
      var t = [];
      for (var v = Math.ceil(min / step) * step; v <= max + 1e-9; v += step) t.push(+v.toFixed(10));
      return { t: t, step: step };
    }
    function hull(pts) {
      if (pts.length < 3) return pts;
      pts = pts.slice().sort(function (a, b) { return a[0] - b[0] || a[1] - b[1]; });
      function cross(o, a, b) { return (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0]); }
      var lo = [], up = [];
      pts.forEach(function (p) { while (lo.length >= 2 && cross(lo[lo.length - 2], lo[lo.length - 1], p) <= 0) lo.pop(); lo.push(p); });
      pts.slice().reverse().forEach(function (p) { while (up.length >= 2 && cross(up[up.length - 2], up[up.length - 1], p) <= 0) up.pop(); up.push(p); });
      return lo.slice(0, -1).concat(up.slice(0, -1));
    }
    function shape(t, x, y, r) {
      if (t.method === 'Inyección') return '<path class="mark" d="M' + x + ' ' + (y - r - 1) + 'L' + (x + r + 1) + ' ' + y + 'L' + x + ' ' + (y + r + 1) + 'L' + (x - r - 1) + ' ' + y + 'Z"/>';
      return '<circle class="mark" cx="' + x + '" cy="' + y + '" r="' + r + '"/>';
    }
    function drawChart() {
      var W = Math.max(300, exChart.clientWidth), narrow = W < 560;
      var H = Math.round(narrow ? W * 0.9 : Math.min(460, W * 0.62));
      var m = { l: narrow ? 46 : 58, r: 16, t: 18, b: narrow ? 44 : 48 };
      var pw = W - m.l - m.r, ph = H - m.t - m.b;
      var isPca = state.view === 'pca';
      var data = TRT.filter(function (t) { return !isPca || t.pca; });
      function gx(t) { return isPca ? t.pca[0] : t[state.x][0]; }
      function gy(t) { return isPca ? t.pca[1] : t[state.y][0]; }
      var xs = [], ys = [];
      data.forEach(function (t) {
        var sx = !isPca && state.sd ? t[state.x][1] : 0, sy = !isPca && state.sd ? t[state.y][1] : 0;
        xs.push(gx(t) - sx, gx(t) + sx); ys.push(gy(t) - sy, gy(t) + sy);
      });
      var x0 = Math.min.apply(null, xs), x1 = Math.max.apply(null, xs), y0 = Math.min.apply(null, ys), y1 = Math.max.apply(null, ys);
      var px = (x1 - x0) * 0.1, py = (y1 - y0) * 0.12;
      x0 -= px; x1 += px; y0 -= py; y1 += py;
      var tx = niceTicks(x0, x1, narrow ? 4 : 6), ty = niceTicks(y0, y1, 5);
      function X(v) { return m.l + (v - x0) / (x1 - x0) * pw; }
      function Y(v) { return m.t + (1 - (v - y0) / (y1 - y0)) * ph; }
      var dec = tx.step < 1 ? 1 : 0, decY = ty.step < 1 ? 1 : 0;
      var s = '';
      tx.t.forEach(function (v) {
        s += '<line class="grid" x1="' + X(v) + '" x2="' + X(v) + '" y1="' + m.t + '" y2="' + (m.t + ph) + '"/>';
        s += '<text class="tick" x="' + X(v) + '" y="' + (m.t + ph + 16) + '" text-anchor="middle">' + fmt(v, dec) + '</text>';
      });
      ty.t.forEach(function (v) {
        s += '<line class="grid" x1="' + m.l + '" x2="' + (m.l + pw) + '" y1="' + Y(v) + '" y2="' + Y(v) + '"/>';
        s += '<text class="tick" x="' + (m.l - 6) + '" y="' + (Y(v) + 4) + '" text-anchor="end">' + fmt(v, decY) + '</text>';
      });
      if (x0 < 0 && x1 > 0) s += '<line class="zero" x1="' + X(0) + '" x2="' + X(0) + '" y1="' + m.t + '" y2="' + (m.t + ph) + '"/>';
      if (y0 < 0 && y1 > 0) s += '<line class="zero" x1="' + m.l + '" x2="' + (m.l + pw) + '" y1="' + Y(0) + '" y2="' + Y(0) + '"/>';
      var xl = isPca ? 'Dim1 (82,5 %)' : VARS[state.x].axis, yl = isPca ? 'Dim2 (17,3 %)' : VARS[state.y].axis;
      s += '<text class="axis" x="' + (m.l + pw / 2) + '" y="' + (H - 6) + '" text-anchor="middle">' + esc(xl) + '</text>';
      s += '<text class="axis" transform="translate(12 ' + (m.t + ph / 2) + ') rotate(-90)" text-anchor="middle">' + esc(yl) + '</text>';
      if (isPca) {
        s += '<text class="hint" x="' + (m.l + 6) + '" y="' + (m.t + ph - 6) + '">← más eficacia</text>';
        s += '<text class="hint" x="' + (m.l + pw - 6) + '" y="' + (m.t + ph - 6) + '" text-anchor="end">menos eficacia →</text>';
        s += '<text class="hint" x="' + (m.l + pw - 6) + '" y="' + (m.t + 14) + '" text-anchor="end">↑ menos persistencia</text>';
        [1, 2].forEach(function (c) {
          var hp = hull(data.filter(function (t) { return t.cluster === c; }).map(function (t) { return [X(gx(t)), Y(gy(t))]; }));
          s += '<polygon class="hull hull--c' + c + '" points="' + hp.map(function (p) { return p[0].toFixed(1) + ',' + p[1].toFixed(1); }).join(' ') + '"/>';
        });
      }
      if (!isPca && state.sd) {
        data.forEach(function (t) {
          var cx = X(gx(t)), cy = Y(gy(t)), ax = X(gx(t) - t[state.x][1]), bx = X(gx(t) + t[state.x][1]), ay = Y(gy(t) - t[state.y][1]), by = Y(gy(t) + t[state.y][1]);
          s += '<g class="ebar c' + t.cluster + '"><line x1="' + ax + '" x2="' + bx + '" y1="' + cy + '" y2="' + cy + '"/><line x1="' + cx + '" x2="' + cx + '" y1="' + ay + '" y2="' + by + '"/></g>';
        });
      }
      /* Etiquetas con un ajuste sencillo para que no se pisen. */
      var labels = data.map(function (t) {
        var cx = X(gx(t)), cy = Y(gy(t)), wTxt = t.short.length * 6.4 + 4, right = cx + 12 + wTxt < m.l + pw;
        return { t: t, x: right ? cx + 11 : cx - 11, y: cy + 4, a: right ? 'start' : 'end', w: wTxt, x0: right ? cx + 11 : cx - 11 - wTxt };
      });
      for (var it = 0; it < 60; it++) {
        for (var i = 0; i < labels.length; i++) for (var j = i + 1; j < labels.length; j++) {
          var A = labels[i], B = labels[j];
          var overlapX = A.x0 < B.x0 + B.w && B.x0 < A.x0 + A.w, dy = B.y - A.y;
          if (overlapX && Math.abs(dy) < 13) { var push = (13 - Math.abs(dy)) / 2 + 0.2, sg = dy >= 0 ? 1 : -1; A.y -= push * sg; B.y += push * sg; }
        }
      }
      var active = state.hover || state.sel;
      data.forEach(function (t, i) {
        var cx = X(gx(t)), cy = Y(gy(t)), L = labels[i], on = t.id === active;
        var lbl = t.name + ' ' + t.dose + ', ' + t.method + (isPca ? ', Dim1 ' + fmt(t.pca[0], 2) + ', Dim2 ' + fmt(t.pca[1], 2) : ', ' + VARS[state.x].label + ' ' + fmt(gx(t)) + ', ' + VARS[state.y].label + ' ' + fmt(gy(t)));
        s += '<g class="pt c' + t.cluster + (t.id === state.sel ? ' is-sel' : '') + (on ? ' is-on' : '') + '" data-id="' + t.id + '" tabindex="0" role="button" aria-label="' + esc(lbl) + '">' +
          '<circle class="halo" cx="' + cx + '" cy="' + cy + '" r="15"/>' + shape(t, cx, cy, on ? 8 : 6.5) +
          '<text class="lbl" x="' + L.x + '" y="' + L.y.toFixed(1) + '" text-anchor="' + L.a + '">' + esc(t.short) + '</text></g>';
      });
      exChart.innerHTML = '<svg viewBox="0 0 ' + W + ' ' + H + '" width="' + W + '" height="' + H + '" role="group" aria-label="' + (isPca ? 'PCA de los tratamientos con dos clústeres' : 'Dispersión de ' + esc(VARS[state.x].axis) + ' frente a ' + esc(VARS[state.y].axis)) + '">' + s + '</svg>';
    }
    function drawDetail() {
      var t = byId[state.hover || state.sel];
      var html = '<p class="widget-label">Tratamiento seleccionado</p><h5 class="detail__name">' + esc(t.name) + '</h5>';
      html += '<p class="detail__meta"><span class="tag">' + esc(t.method === '—' ? 'Control sin tratar' : t.method + ' · ' + t.dose) + '</span>';
      if (t.cluster) html += '<span class="tag tag--c' + t.cluster + '">Clúster ' + t.cluster + '</span>';
      if (t.verdict) html += '<span class="badge ' + VERDICT[t.verdict][1] + '">' + VERDICT[t.verdict][0] + '</span>';
      html += '</p><dl class="detail__metrics">';
      Object.keys(VARS).forEach(function (k) {
        var vals = TRT.map(function (x) { return x[k][0]; }), lo = Math.min.apply(null, vals), hi = Math.max.apply(null, vals);
        var p = (t[k][0] - lo) / (hi - lo) * 100;
        html += '<div><dt>' + VARS[k].label + '</dt><dd><span class="mono">' + fmt(t[k][0]) + ' ± ' + fmt(t[k][1]) + '</span><sup class="mono">' + t[k][2] + '</sup>' +
          '<span class="meter" aria-hidden="true"><span style="--p:' + p.toFixed(1) + '%"></span></span></dd></div>';
      });
      html += '</dl>';
      html += t.pca ? '<p class="detail__pca mono">Dim1 ' + fmt(t.pca[0], 2) + ' · Dim2 ' + fmt(t.pca[1], 2) + '</p>' : '<p class="detail__pca">No entra en el PCA: el clustering se hizo solo con los tratamientos.</p>';
      html += '<p class="detail__hint">La barra sitúa el valor entre el mínimo y el máximo de los nueve tratamientos.</p>';
      exDetail.innerHTML = html;
    }
    function drawCaption() {
      exCap.textContent = state.view === 'pca'
        ? 'PCA recalculado con las medias de la tabla para los ocho tratamientos, con las variables estandarizadas: reproduce el 82,5 % y el 17,3 % del informe. Dim1 resume la eficacia (AUC, m codo y m final cargan en negativo) y Dim2 sobre todo la persistencia (Δ codo carga −0,84). Los polígonos son los dos clústeres de k-medias.'
        : 'Medias por tratamiento de la tabla de la presentación' + (state.sd ? ', con barras de ± una desviación típica' : '') + '. El control sin tratar aparece como referencia en gris.';
    }
    function drawTable() {
      var rows = TRT.slice().sort(function (a, b) {
        var k = state.sort, va, vb;
        if (k === 'name') { va = a.short; vb = b.short; return va.localeCompare(vb, 'es') * state.dir; }
        if (k === 'method') { va = a.method; vb = b.method; return va.localeCompare(vb, 'es') * state.dir || a.short.localeCompare(b.short, 'es'); }
        if (k === 'cluster') { return ((a.cluster || 9) - (b.cluster || 9)) * state.dir || b.auc[0] - a.auc[0]; }
        return (a[k][0] - b[k][0]) * state.dir;
      });
      tbody.innerHTML = rows.map(function (t) {
        function cell(k) { return '<td class="num"><span class="mono">' + fmt(t[k][0]) + ' ± ' + fmt(t[k][1]) + '</span><sup class="mono">' + t[k][2] + '</sup></td>'; }
        return '<tr data-id="' + t.id + '"' + (t.id === state.sel ? ' class="is-sel"' : '') + '>' +
          '<td><button type="button" class="row-pick">' + esc(t.name) + '</button><small>' + esc(t.dose) + '</small></td>' +
          '<td>' + esc(t.method) + '</td>' + cell('auc') + cell('fin') + cell('elb') + cell('dlt') +
          '<td>' + (t.cluster ? '<span class="tag tag--c' + t.cluster + '">' + t.cluster + '</span>' : '—') + (t.verdict ? ' <span class="badge ' + VERDICT[t.verdict][1] + '">' + VERDICT[t.verdict][0] + '</span>' : '') + '</td></tr>';
      }).join('');
      $$('#ex-table th').forEach(function (th) {
        var b = $('button', th), k = b && b.getAttribute('data-sort');
        th.setAttribute('aria-sort', k === state.sort ? (state.dir > 0 ? 'ascending' : 'descending') : 'none');
      });
    }
    function render() { drawChart(); drawDetail(); drawCaption(); }
    function select(id) { state.sel = id; state.hover = null; render(); drawTable(); }

    seg($('.explorer__controls .seg', ex), 'data-view', function (v) {
      state.view = v; $('#ex-axes').hidden = v !== 'vars';
      if (v === 'pca' && !byId[state.sel].pca) state.sel = 'CONFIDOR_750_FOLIAR';
      render(); drawTable();
    });
    selX.addEventListener('change', function () { state.x = selX.value; render(); });
    selY.addEventListener('change', function () { state.y = selY.value; render(); });
    chkSd.addEventListener('change', function () { state.sd = chkSd.checked; render(); });

    exChart.addEventListener('click', function (e) { var g = e.target.closest('.pt'); if (g) select(g.getAttribute('data-id')); });
    exChart.addEventListener('keydown', function (e) {
      var g = e.target.closest('.pt');
      if (g && (e.key === 'Enter' || e.key === ' ')) { e.preventDefault(); select(g.getAttribute('data-id')); var n = $('.pt[data-id="' + state.sel + '"]', exChart); if (n) n.focus(); }
    });
    exChart.addEventListener('mouseover', function (e) {
      var g = e.target.closest('.pt'), id = g && g.getAttribute('data-id');
      if (id && id !== state.hover) { state.hover = id; drawDetail(); $$('.pt', exChart).forEach(function (p) { p.classList.toggle('is-on', p.getAttribute('data-id') === id); }); }
    });
    exChart.addEventListener('mouseleave', function () {
      state.hover = null; drawDetail();
      $$('.pt', exChart).forEach(function (p) { p.classList.toggle('is-on', p.getAttribute('data-id') === state.sel); });
    });
    $('#ex-table').addEventListener('click', function (e) {
      var sb = e.target.closest('button[data-sort]');
      if (sb) {
        var k = sb.getAttribute('data-sort');
        if (state.sort === k) state.dir *= -1; else { state.sort = k; state.dir = (k === 'name' || k === 'method' || k === 'cluster') ? 1 : -1; }
        drawTable(); return;
      }
      var row = e.target.closest('tr[data-id]');
      if (row) {
        var id = row.getAttribute('data-id');
        if (state.view === 'pca' && !byId[id].pca) { state.view = 'vars'; $('#ex-axes').hidden = false; $$('.explorer__controls .seg button', ex).forEach(function (b) { b.setAttribute('aria-pressed', b.getAttribute('data-view') === 'vars' ? 'true' : 'false'); }); }
        select(id);
      }
    });
    $$('.board [data-select]').forEach(function (b) {
      b.addEventListener('click', function () {
        select(b.getAttribute('data-select'));
        ex.scrollIntoView({ behavior: window.matchMedia('(prefers-reduced-motion: reduce)').matches ? 'auto' : 'smooth', block: 'start' });
      });
    });
    onResize(exChart, drawChart);
    render(); drawTable();
  }

  /* =====================================================================
     Lightbox con navegación entre figuras
     ===================================================================== */
  var lb = $('#lightbox'), lbImg = $('#lightbox-img'), lbCap = $('#lightbox-caption'), lbItems = [], lbIdx = 0, lastFocus = null;
  function lbShow(i) {
    lbIdx = (i + lbItems.length) % lbItems.length;
    var it = lbItems[lbIdx];
    lbImg.src = it.src; lbImg.alt = it.alt || '';
    lbCap.textContent = it.caption + (lbItems.length > 1 ? '  ·  ' + (lbIdx + 1) + ' / ' + lbItems.length : '');
    $$('.lightbox__nav', lb).forEach(function (b) { b.hidden = lbItems.length < 2; });
  }
  function lbOpen(items, i) {
    lbItems = items; lastFocus = document.activeElement;
    lb.hidden = false; document.body.classList.add('lightbox-open');
    lbShow(i); $('.lightbox__close', lb).focus();
  }
  function lbClose() {
    lb.hidden = true; lbImg.src = 'data:,';
    document.body.classList.remove('lightbox-open');
    if (lastFocus && lastFocus.focus) lastFocus.focus();
  }
  function visible(el) { return !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length); }
  function figureItems() {
    return $$('a.zoom, a.zoom-link').filter(function (a) { return visible(a); }).map(function (a) {
      var im = $('img', a), fig = a.closest('figure'), cap = fig && $('figcaption', fig);
      return { el: a, src: a.getAttribute('href'), alt: im ? im.alt : '', caption: cap ? cap.textContent.trim() : (a.getAttribute('data-caption') || '') };
    });
  }
  document.addEventListener('click', function (e) {
    var a = e.target.closest('a.zoom, a.zoom-link');
    if (!a || !lb) return;
    e.preventDefault();
    var items = figureItems(), i = 0;
    items.forEach(function (it, j) { if (it.el === a) i = j; });
    lbOpen(items, i);
  });
  if (lb) {
    lb.addEventListener('click', function (e) {
      if (e.target.closest('.lightbox__nav--prev')) return lbShow(lbIdx - 1);
      if (e.target.closest('.lightbox__nav--next')) return lbShow(lbIdx + 1);
      if (e.target === lb || e.target.closest('.lightbox__close')) lbClose();
    });
    document.addEventListener('keydown', function (e) {
      if (lb.hidden) return;
      if (e.key === 'Escape') lbClose();
      if (e.key === 'ArrowRight') lbShow(lbIdx + 1);
      if (e.key === 'ArrowLeft') lbShow(lbIdx - 1);
      if (e.key === 'Tab') {
        var f = $$('button:not([hidden])', lb);
        var first = f[0], last = f[f.length - 1];
        if (e.shiftKey && document.activeElement === first) { e.preventDefault(); last.focus(); }
        else if (!e.shiftKey && document.activeElement === last) { e.preventDefault(); first.focus(); }
      }
    });
  }

  /* =====================================================================
     Carrusel de diapositivas (flechas, miniaturas, deslizar y ampliar)
     ===================================================================== */
  var carousel = $('[data-carousel]');
  if (carousel) {
    var TOTAL = 24, idx = 0, img = $('#carousel-img'), counter = $('#carousel-counter'), thumbs = $('#carousel-thumbs');
    var TITLES = {
      1: 'Portada: Efficacy evaluation of ISOCLAST against Philaenus spumarius on weeds in semi-field, Italy',
      2: 'Introducción: Philaenus spumarius como vector de Xylella fastidiosa',
      3: 'Objetivos del estudio',
      4: 'Eficacia por tratamiento en 2017', 5: 'Variabilidad entre parcelas en 2017',
      6: 'Eficacia por tratamiento en 2018', 7: 'Variabilidad entre parcelas en 2018',
      8: 'Foliar frente a inyección en 2017', 9: 'Foliar frente a inyección en 2018',
      10: 'Curvas de persistencia en 2017', 11: 'Curvas de persistencia en 2018',
      20: 'PCA y clústeres de eficacia y persistencia', 21: 'Conclusiones finales', 22: 'Conclusiones finales',
      23: 'Apéndice: tratamientos de 2017', 24: 'Apéndice: tratamientos de 2018'
    };
    function title(i) { return TITLES[i] || (i >= 12 && i <= 19 ? 'Tabla de persistencia con agrupación estadística' : ''); }
    function src(i) { return 'assets/slides/slide-' + String(i + 1).padStart(2, '0') + '.png'; }
    function alt(i) { return 'Diapositiva ' + (i + 1) + ' de ' + TOTAL + (title(i + 1) ? ': ' + title(i + 1) + '.' : '.'); }
    function show(i) {
      idx = (i + TOTAL) % TOTAL;
      img.src = src(idx); img.alt = alt(idx);
      counter.textContent = (idx + 1) + ' / ' + TOTAL;
      Array.prototype.forEach.call(thumbs.children, function (t, j) { t.classList.toggle('is-active', j === idx); t.setAttribute('aria-current', j === idx ? 'true' : 'false'); });
      var at = thumbs.children[idx];
      if (at) thumbs.scrollTo({ left: at.offsetLeft - thumbs.clientWidth / 2 + at.clientWidth / 2, behavior: 'smooth' });
    }
    for (var i = 0; i < TOTAL; i++) {
      var b = document.createElement('button');
      b.type = 'button'; b.className = 'carousel__thumb' + (i === 0 ? ' is-active' : '');
      b.setAttribute('aria-label', 'Ir a la diapositiva ' + (i + 1)); b.setAttribute('role', 'listitem');
      var t = document.createElement('img');
      t.src = src(i); t.alt = ''; t.loading = 'lazy'; t.width = 1280; t.height = 720;
      b.appendChild(t);
      b.addEventListener('click', (function (n) { return function () { show(n); }; })(i));
      thumbs.appendChild(b);
    }
    $('[data-prev]', carousel).addEventListener('click', function () { show(idx - 1); });
    $('[data-next]', carousel).addEventListener('click', function () { show(idx + 1); });
    carousel.addEventListener('keydown', function (e) {
      if (!lb.hidden) return;
      if (e.key === 'ArrowRight') { e.preventDefault(); show(idx + 1); }
      if (e.key === 'ArrowLeft') { e.preventDefault(); show(idx - 1); }
    });
    var x0 = null;
    var vp = $('.carousel__viewport', carousel);
    vp.addEventListener('touchstart', function (e) { x0 = e.touches[0].clientX; }, { passive: true });
    vp.addEventListener('touchend', function (e) {
      if (x0 === null) return;
      var dx = e.changedTouches[0].clientX - x0; x0 = null;
      if (Math.abs(dx) > 40) { show(idx + (dx < 0 ? 1 : -1)); vp.dataset.swiped = '1'; setTimeout(function () { vp.dataset.swiped = ''; }, 300); }
    });
    $('#carousel-open').addEventListener('click', function () {
      if (vp.dataset.swiped) return;
      var items = [];
      for (var k = 0; k < TOTAL; k++) items.push({ src: src(k), alt: alt(k), caption: 'Diapositiva ' + (k + 1) + (title(k + 1) ? ' · ' + title(k + 1) : '') });
      lbOpen(items, idx);
    });
  }
})();
