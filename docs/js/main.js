(function () {
  'use strict';

  /* ---------- tema claro / oscuro ---------- */
  var root = document.documentElement;
  var toggle = document.getElementById('theme-toggle');
  function readTheme() { try { return localStorage.getItem('theme'); } catch (e) { return null; } }
  function saveTheme(v) { try { localStorage.setItem('theme', v); } catch (e) { /* sin almacenamiento */ } }
  var saved = readTheme();
  if (saved === 'dark' || saved === 'light') root.setAttribute('data-theme', saved);
  if (toggle) {
    toggle.addEventListener('click', function () {
      var current = root.getAttribute('data-theme');
      var systemDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
      var isDark = current ? current === 'dark' : systemDark;
      var next = isDark ? 'light' : 'dark';
      root.setAttribute('data-theme', next);
      saveTheme(next);
    });
  }

  /* ---------- menú móvil ---------- */
  var navToggle = document.querySelector('.nav__toggle');
  var navList = document.getElementById('nav-list');
  if (navToggle && navList) {
    navToggle.addEventListener('click', function () {
      var open = navList.classList.toggle('is-open');
      navToggle.setAttribute('aria-expanded', open ? 'true' : 'false');
    });
    navList.addEventListener('click', function (e) {
      if (e.target.tagName === 'A') { navList.classList.remove('is-open'); navToggle.setAttribute('aria-expanded', 'false'); }
    });
  }

  /* ---------- scroll-spy ---------- */
  var links = Array.prototype.slice.call(document.querySelectorAll('.nav__list a[href^="#"]'));
  var sections = links.map(function (a) { return document.querySelector(a.getAttribute('href')); }).filter(Boolean);
  if ('IntersectionObserver' in window && sections.length) {
    var active = null;
    var io = new IntersectionObserver(function (entries) {
      entries.forEach(function (en) {
        if (en.isIntersecting) {
          active = en.target.id;
          links.forEach(function (a) { a.classList.toggle('is-active', a.getAttribute('href') === '#' + active); });
        }
      });
    }, { rootMargin: '-40% 0px -55% 0px', threshold: 0 });
    sections.forEach(function (s) { io.observe(s); });
  }

  /* ---------- pestañas ---------- */
  document.querySelectorAll('[data-tabs]').forEach(function (tabs) {
    var btns = Array.prototype.slice.call(tabs.querySelectorAll('[role="tab"]'));
    var panels = Array.prototype.slice.call(tabs.querySelectorAll('[role="tabpanel"]'));
    function select(i) {
      btns.forEach(function (b, j) {
        var on = i === j;
        b.setAttribute('aria-selected', on ? 'true' : 'false');
        b.tabIndex = on ? 0 : -1;
      });
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

  /* ---------- carrusel de diapositivas ---------- */
  var carousel = document.querySelector('[data-carousel]');
  if (carousel) {
    var TOTAL = 24;
    var img = document.getElementById('carousel-img');
    var counter = document.getElementById('carousel-counter');
    var thumbs = document.getElementById('carousel-thumbs');
    var alts = {
      1: 'Portada: Efficacy evaluation of ISOCLAST against Philaenus spumarius on weeds in semi-field, Italy, 2017.',
      2: 'Introducción: Philaenus spumarius como vector de Xylella fastidiosa.',
      3: 'Objetivos del estudio: tratamiento más eficaz, método de aplicación y persistencia.'
    };
    var idx = 0;
    function src(i) { return 'assets/slides/slide-' + String(i + 1).padStart(2, '0') + '.png'; }
    function show(i) {
      idx = (i + TOTAL) % TOTAL;
      img.src = src(idx);
      img.alt = 'Diapositiva ' + (idx + 1) + ' de ' + TOTAL + (alts[idx + 1] ? ': ' + alts[idx + 1] : '.');
      counter.textContent = (idx + 1) + ' / ' + TOTAL;
      Array.prototype.forEach.call(thumbs.children, function (t, j) { t.classList.toggle('is-active', j === idx); });
      var activeThumb = thumbs.children[idx];
      if (activeThumb && activeThumb.scrollIntoView) activeThumb.scrollIntoView({ block: 'nearest', inline: 'center', behavior: 'smooth' });
    }
    for (var i = 0; i < TOTAL; i++) {
      var b = document.createElement('button');
      b.type = 'button';
      b.className = 'carousel__thumb' + (i === 0 ? ' is-active' : '');
      b.setAttribute('aria-label', 'Ir a la diapositiva ' + (i + 1));
      b.setAttribute('role', 'listitem');
      var t = document.createElement('img');
      t.src = src(i); t.alt = ''; t.loading = 'lazy'; t.width = 1280; t.height = 720;
      b.appendChild(t);
      b.addEventListener('click', (function (n) { return function () { show(n); }; })(i));
      thumbs.appendChild(b);
    }
    carousel.querySelector('[data-prev]').addEventListener('click', function () { show(idx - 1); });
    carousel.querySelector('[data-next]').addEventListener('click', function () { show(idx + 1); });
    carousel.addEventListener('keydown', function (e) {
      if (e.key === 'ArrowRight') { e.preventDefault(); show(idx + 1); }
      if (e.key === 'ArrowLeft') { e.preventDefault(); show(idx - 1); }
    });
  }

  /* ---------- lightbox ---------- */
  var lb = document.getElementById('lightbox');
  var lbImg = document.getElementById('lightbox-img');
  var lbCap = document.getElementById('lightbox-caption');
  var lastFocus = null;
  function openLb(href, alt, caption) {
    lbImg.src = href; lbImg.alt = alt || '';
    lbCap.textContent = caption || '';
    lb.hidden = false;
    document.body.classList.add('lightbox-open');
    lastFocus = document.activeElement;
    lb.querySelector('.lightbox__close').focus();
  }
  function closeLb() {
    lb.hidden = true; lbImg.src = '';
    document.body.classList.remove('lightbox-open');
    if (lastFocus && lastFocus.focus) lastFocus.focus();
  }
  if (lb) {
    document.querySelectorAll('.figure .zoom').forEach(function (a) {
      a.addEventListener('click', function (e) {
        e.preventDefault();
        var im = a.querySelector('img');
        var fig = a.closest('figure');
        var cap = fig && fig.querySelector('figcaption');
        openLb(a.getAttribute('href'), im && im.alt, cap && cap.textContent);
      });
    });
    lb.addEventListener('click', function (e) { if (e.target === lb || e.target.classList.contains('lightbox__close')) closeLb(); });
    document.addEventListener('keydown', function (e) { if (e.key === 'Escape' && !lb.hidden) closeLb(); });
  }
})();
