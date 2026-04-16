function setExternalMenu(el, x) {
  const gd = el;
  const cfg = x.roboplot_externalmenu;
  if (!cfg || !cfg.col) return;

  const uniq = arr => Array.from(new Set(arr));
  const asArr = v => Array.isArray(v) ? v : (v == null ? [] : [v]);

  const maxItems = cfg["max-items"] ?? null;
  const layoutMode = cfg.layout || cfg.position || 'popup'; // 'popup' | 'side' | 'below'
  const noCheckmarks = (cfg.checkmark?.size === 0);

  function normVal(v) {
    if (v == null) return [];
    if (Array.isArray(v)) return v.filter(x => x != null).map(x => '' + x);
    return ['' + v];
  }

  // ---- Cleanup previous menu/layout if rerendered ----
  const prevOuterLayout = gd.closest?.('.roboplot-externalmenu-layout');
  if (prevOuterLayout) {
    const prevSlot = prevOuterLayout.querySelector(':scope > .roboplot-externalmenu-plot');
    const prevRoot = prevSlot?.firstElementChild || gd;
    prevOuterLayout.parentNode.insertBefore(prevRoot, prevOuterLayout);
    prevOuterLayout.remove();
  }

  if (gd._extmenu_originalRootStyle?.el) {
    const root = gd._extmenu_originalRootStyle.el;
    root.style.flex = gd._extmenu_originalRootStyle.flex;
    root.style.minWidth = gd._extmenu_originalRootStyle.minWidth;
    root.style.width = gd._extmenu_originalRootStyle.width;
    root.style.maxWidth = gd._extmenu_originalRootStyle.maxWidth;
    delete gd._extmenu_originalRootStyle;
  }

  if (gd._extmenu_originalStyle) {
    gd.style.flex = gd._extmenu_originalStyle.flex;
    gd.style.minWidth = gd._extmenu_originalStyle.minWidth;
    gd.style.width = gd._extmenu_originalStyle.width;
    gd.style.maxWidth = gd._extmenu_originalStyle.maxWidth;
    delete gd._extmenu_originalStyle;
  }

  if (cfg.id) {
    const prev = gd.querySelector(`#${CSS.escape(cfg.id)}`);
    if (prev) prev.remove();
  }

  const prevChip = gd.querySelector(
    `#${CSS.escape(cfg.id ? cfg.id + '-chip' : 'roboplot-chip')}`
  );
  if (prevChip) prevChip.remove();

  const prevLayout = gd.querySelector(':scope > .roboplot-externalmenu-layout');
  if (prevLayout) {
    const prevPlot = prevLayout.querySelector('.plot-container.plotly, .plot-container');
    if (prevPlot) gd.insertBefore(prevPlot, prevLayout);
    prevLayout.remove();
  }

  if (gd._extmenu_resizeObserver) {
    gd._extmenu_resizeObserver.disconnect();
    delete gd._extmenu_resizeObserver;
  }

  if (gd._extmenu_resizeRaf) {
    cancelAnimationFrame(gd._extmenu_resizeRaf);
    delete gd._extmenu_resizeRaf;
  }

  if (gd._extmenu_plotlyRelayoutHandler) {
    gd.removeListener?.('plotly_relayout', gd._extmenu_plotlyRelayoutHandler);
    delete gd._extmenu_plotlyRelayoutHandler;
  }

  if (gd._extmenu_windowResizeHandler) {
    window.removeEventListener('resize', gd._extmenu_windowResizeHandler);
    delete gd._extmenu_windowResizeHandler;
  }

  // ---- Capture original per-trace arrays ----
  const original = (gd.data || []).map(tr => {
    const full = (tr && tr.meta && tr.meta.extmenu_full) ? tr.meta.extmenu_full : tr;

    const xvals = asArr(full.x ?? []);
    const yvals = asArr(full.y ?? []);
    const n = Math.max(xvals.length, yvals.length);

    const cd = asArr(full.customdata ?? []);
    const filterMulti = [];
    for (let j = 0; j < n; j++) filterMulti.push(normVal(cd[j]));

    const text = asArr(full.text ?? null);
    const hovertext = asArr(full.hovertext ?? null);

    return {
      x: xvals.slice(),
      y: yvals.slice(),
      filterMulti,
      text: (text.length === n) ? text.slice() : null,
      hovertext: (hovertext.length === n) ? hovertext.slice() : null
    };
  });

  gd._extmenu_original = original;

  for (let i = 0; i < (gd.data || []).length; i++) {
    const tr = gd.data[i];
    if (tr && tr.meta && tr.meta.extmenu_full) {
      delete tr.meta.extmenu_full;
      if (Object.keys(tr.meta).length === 0) delete tr.meta;
    }
  }

  const ORIG = gd._extmenu_original || original;

  // ---- Unique filter values across all points ----
  const allF = uniq(
    ORIG.flatMap(o => o.filterMulti.flatMap(vs => vs))
  ).sort((a, b) => a.localeCompare(b));

  // ---- Initial selected logic ----
  function initSelected() {
    if (!maxItems) return allF.slice();

    if (cfg.selected != null) {
      const want = asArr(cfg.selected).filter(v => v != null).map(v => '' + v);
      const out = [];
      const seen = new Set();

      for (const v of want) {
        if (seen.has(v)) continue;
        if (!allF.includes(v)) continue;
        out.push(v);
        seen.add(v);
        if (out.length >= maxItems) break;
      }
      return out;
    }

    const out = [];
    const seen = new Set();
    for (let ti = 0; ti < ORIG.length && out.length < maxItems; ti++) {
      const valsThisTrace = uniq(ORIG[ti].filterMulti.flat());
      for (const v of valsThisTrace) {
        if (seen.has(v)) continue;
        out.push(v);
        seen.add(v);
        if (out.length >= maxItems) break;
      }
    }

    if (out.length === 0) return allF.slice(0, Math.min(maxItems, allF.length));
    return out;
  }

  const selected = new Set(initSelected());

  // ---- Root / container references ----
  gd.style.position = gd.style.position || 'relative';
  gd.style.boxSizing = 'border-box';

  const plotContainer =
    gd.querySelector('.plot-container.plotly') ||
    gd.querySelector('.plot-container');

  if (!plotContainer) return;

  // ---- Layout host ----
  // Popup mode can live inside the graph div because it overlays the plot.
  // Side/below modes wrap the htmlwidget sizing container when it exists. That
  // keeps the menu outside the box htmlwidgets/Plotly measure during resize.
  let layoutHost = null;
  let plotLayoutEl = null;
  let plotSlot = null;
  const htmlwidgetContainer = document.getElementById('htmlwidget_container');
  const plotRoot = (htmlwidgetContainer && htmlwidgetContainer.contains(gd))
    ? htmlwidgetContainer
    : gd;

  if (layoutMode === 'popup') {
    layoutHost = document.createElement('div');
    layoutHost.className = 'roboplot-externalmenu-layout';
    layoutHost.style.boxSizing = 'border-box';
    layoutHost.style.width = '100%';

    plotContainer.parentNode.insertBefore(layoutHost, plotContainer);
    layoutHost.appendChild(plotContainer);
    plotLayoutEl = plotContainer;
  } else {
    gd._extmenu_originalRootStyle = {
      el: plotRoot,
      flex: plotRoot.style.flex,
      minWidth: plotRoot.style.minWidth,
      width: plotRoot.style.width,
      maxWidth: plotRoot.style.maxWidth
    };

    gd._extmenu_originalStyle = {
      flex: gd.style.flex,
      minWidth: gd.style.minWidth,
      width: gd.style.width,
      maxWidth: gd.style.maxWidth
    };

    const parent = plotRoot.parentNode;
    layoutHost = document.createElement('div');
    layoutHost.className = 'roboplot-externalmenu-layout';
    layoutHost.style.boxSizing = 'border-box';
    layoutHost.style.width = '100%';

    plotSlot = document.createElement('div');
    plotSlot.className = 'roboplot-externalmenu-plot';
    plotSlot.style.boxSizing = 'border-box';
    plotSlot.style.minWidth = '0';

    parent.insertBefore(layoutHost, plotRoot);
    layoutHost.appendChild(plotSlot);
    plotSlot.appendChild(plotRoot);
    plotLayoutEl = plotSlot;

    plotRoot.style.width = '100%';
    plotRoot.style.maxWidth = '100%';
    plotRoot.style.minWidth = '0';

    gd.style.width = '100%';
    gd.style.maxWidth = '100%';
    gd.style.minWidth = '0';
  }

  // ---- Menu wrapper ----
  const wrap = document.createElement('div');
  wrap.id = cfg.id || 'roboplot-externalmenu';
  layoutHost.appendChild(wrap);

  // ---- Responsive layout helpers ----
  const gapPx = cfg.layout_gap || 12;
  const sideMenuWidth = cfg.menu_width || 140;
  const responsiveSideToBelow = cfg.responsive_side_to_below !== false;
  const plotMinWidth = cfg.plot_min_width || 360;
  let currentLayoutMode = null;
  let resizeRaf = null;

  function effectiveLayoutMode() {
    if (layoutMode !== 'side' || !responsiveSideToBelow) return layoutMode;

    // Use the outer available width, not gd. In side mode gd is the shrunken
    // plot flex item; in below mode gd is full-width. Reading gd here makes the
    // breakpoint feed back into itself and can cause side/below flicker.
    const hostParent = layoutHost.parentElement;
    const w =
      hostParent?.clientWidth ||
      hostParent?.getBoundingClientRect().width ||
      layoutHost.clientWidth ||
      layoutHost.getBoundingClientRect().width ||
      gd.clientWidth ||
      gd.getBoundingClientRect().width ||
      0;

    return w < (sideMenuWidth + gapPx + plotMinWidth) ? 'below' : 'side';
  }

  function applyMenuLayout() {
    const mode = effectiveLayoutMode();
    const modeChanged = mode !== currentLayoutMode;
    currentLayoutMode = mode;

    // reset host
    layoutHost.style.display = '';
    layoutHost.style.flexDirection = '';
    layoutHost.style.alignItems = '';
    layoutHost.style.gap = '';
    layoutHost.style.width = '100%';

    // reset plot layout element
    plotLayoutEl.style.gridColumn = '';
    plotLayoutEl.style.gridRow = '';
    plotLayoutEl.style.flex = '';
    plotLayoutEl.style.minWidth = '';
    plotLayoutEl.style.overflow = '';
    if (plotLayoutEl !== gd) {
      plotLayoutEl.style.width = '';
      plotLayoutEl.style.maxWidth = '';
    }
    plotLayoutEl.style.boxSizing = 'border-box';

    plotContainer.style.gridColumn = '';
    plotContainer.style.gridRow = '';
    plotContainer.style.flex = '';
    plotContainer.style.minWidth = '';
    plotContainer.style.boxSizing = 'border-box';

    // reset menu
    wrap.style.gridColumn = '';
    wrap.style.gridRow = '';
    wrap.style.flex = '';

    if (mode === 'popup') {
      layoutHost.style.display = 'block';

      plotLayoutEl.style.width = '100%';
      plotLayoutEl.style.maxWidth = '100%';

      wrap.style.cssText = `
        position:absolute;
        top:6%;
        left:6%;
        width:${cfg.menu_width_pct || 50}%;
        max-height:${cfg.menu_maxheight_pct || 80}%;
        overflow:auto;
        z-index:9999;
        display:none;
        background:${cfg.box?.background || '#fff'};
        border:${cfg.box?.border ? `${cfg.box.border_width}px solid ${cfg.box.border}` : 'none'};
        box-shadow:0 4px 8px rgba(0,0,0,0.18);
        font-family:${cfg.box?.font?.family || 'inherit'};
        color:${cfg.box?.font?.color || 'inherit'};
        font-size:${cfg.btn?.font?.size || 12}px;
        padding:10px;
        border-radius:8px;
        box-sizing:border-box;
      `;
    } else if (mode === 'side') {
      const sideMenuTopMargin = gd.layout?.margin?.t || 0;

      layoutHost.style.display = 'flex';
      layoutHost.style.flexDirection = 'row';
      layoutHost.style.alignItems = 'flex-start';
      layoutHost.style.gap = `${gapPx}px`;

      plotLayoutEl.style.flex = '1 1 0';
      plotLayoutEl.style.minWidth = '0';
      plotLayoutEl.style.width = 'auto';
      plotLayoutEl.style.maxWidth = 'none';
      plotLayoutEl.style.overflow = 'hidden';

      wrap.style.cssText = `
        position:relative;
        flex:0 0 ${sideMenuWidth}px;
        width:${sideMenuWidth}px;
        max-width:${sideMenuWidth}px;
        margin-top:${sideMenuTopMargin}px;
        max-height:${cfg.menu_maxheight || 500}px;
        overflow:auto;
        display:block;
        background:${cfg.box?.background || '#fff'};
        border:${cfg.box?.border ? `${cfg.box.border_width}px solid ${cfg.box.border}` : 'none'};
        box-shadow:0 4px 8px rgba(0,0,0,0.12);
        font-family:${cfg.box?.font?.family || 'inherit'};
        color:${cfg.box?.font?.color || 'inherit'};
        font-size:${cfg.box?.font?.size || 12}px;
        padding:10px;
        border-radius:8px;
        box-sizing:border-box;
        align-self:flex-start;
        z-index:1;
      `;
    } else {
      layoutHost.style.display = 'flex';
      layoutHost.style.flexDirection = 'column';
      layoutHost.style.alignItems = 'stretch';
      layoutHost.style.gap = `${gapPx}px`;

      plotLayoutEl.style.flex = '0 0 auto';
      plotLayoutEl.style.minWidth = '0';
      plotLayoutEl.style.width = '100%';
      plotLayoutEl.style.maxWidth = '100%';

      wrap.style.cssText = `
        position:relative;
        width:100%;
        max-height:${cfg.menu_maxheight || 320}px;
        overflow:auto;
        display:block;
        background:${cfg.box?.background || '#fff'};
        border:${cfg.box?.border ? `${cfg.box.border_width}px solid ${cfg.box.border}` : 'none'};
        box-shadow:0 4px 8px rgba(0,0,0,0.12);
        font-family:${cfg.box?.font?.family || 'inherit'};
        color:${cfg.box?.font?.color || 'inherit'};
        font-size:${cfg.box?.font?.size || 12}px;
        padding:10px;
        border-radius:8px;
        box-sizing:border-box;
      `;
    }

    requestAnimationFrame(() => {
      try {
        if (mode === 'side' && plotLayoutEl !== gd) {
          const slotRect = plotLayoutEl.getBoundingClientRect();
          const gdRect = gd.getBoundingClientRect();
          if (slotRect.width > 0 && gdRect.height > 0) {
            Plotly.relayout(gd, {width: slotRect.width, height: gdRect.height});
          } else {
            Plotly.Plots.resize(gd);
          }
        } else {
          Plotly.Plots.resize(gd);
        }
      } catch (e) {}
    });

    return modeChanged;
  }

  applyMenuLayout();

  // ---- Header / drag bar (popup only) ----
  let header = null;

  if (layoutMode === 'popup') {
    header = document.createElement('div');
    header.className = 'drag-bar';
    header.style.cssText = `
      width:100%;
      user-select:none;
      margin-bottom:6px;
      font-weight:bold;
      display:flex;
      align-items:center;
      justify-content:space-between;
      gap:8px;
      cursor:move;
    `;

    const title = document.createElement('div');
    title.innerHTML = cfg.grip || cfg.col || '';
    title.style.minWidth = '0';
    header.appendChild(title);

    wrap.prepend(header);

    let isDragging = false;
    let startX, startY, startLeft, startTop;

    header.addEventListener('mousedown', (e) => {
      const isClose = e.target && (
        e.target === header._closeBtn ||
        (header._closeBtn && header._closeBtn.contains(e.target))
      );
      if (isClose) return;

      isDragging = true;
      startX = e.clientX;
      startY = e.clientY;

      const rect = wrap.getBoundingClientRect();
      const parentRect = gd.getBoundingClientRect();

      startLeft = rect.left - parentRect.left;
      startTop = rect.top - parentRect.top;

      document.body.style.userSelect = 'none';
    });

    const mouseMove = (e) => {
      if (!isDragging) return;

      const dx = e.clientX - startX;
      const dy = e.clientY - startY;

      const parentRect = gd.getBoundingClientRect();

      let newLeft = startLeft + dx;
      let newTop = startTop + dy;

      newLeft = Math.max(0, Math.min(newLeft, parentRect.width - wrap.offsetWidth));
      newTop = Math.max(0, Math.min(newTop, parentRect.height - wrap.offsetHeight));

      wrap.style.left = newLeft + 'px';
      wrap.style.top = newTop + 'px';
    };

    const mouseUp = () => {
      isDragging = false;
      document.body.style.userSelect = '';
    };

    document.addEventListener('mousemove', mouseMove);
    document.addEventListener('mouseup', mouseUp);

    const closeBtn = document.createElement('span');
    closeBtn.innerHTML = cfg.close || '&times;';
    closeBtn.style.cssText = `
      cursor:pointer;
      line-height:1;
      margin-left:10px;
      border-radius:50%;
      flex:0 0 auto;
      position:sticky;
      top:0;
    `;
    if (cfg.btn?.background) closeBtn.style.background = cfg.btn.background;
    if (cfg.btn?.font?.size) closeBtn.style.fontSize = cfg.btn.font.size + 'px';
    if (cfg.btn?.font?.color) closeBtn.style.color = cfg.btn.font.color;
    if (cfg.btn?.font?.family) closeBtn.style.fontFamily = cfg.btn.font.family;

    closeBtn.addEventListener('click', () => {
      wrap.style.display = 'none';
    });

    header.appendChild(closeBtn);
    header._closeBtn = closeBtn;
  }

  // ---- Limit indicator / warning ----
  const limitBox = document.createElement('div');
  limitBox.style.cssText = `
    width:100%;
    margin:4px 0 10px 0;
    font-size:0.95em;
    opacity:0.9;
  `;

  const limitText = document.createElement('div');
  const limitWarn = document.createElement('div');
  limitWarn.style.cssText = `margin-top:4px; display:none;`;

  limitBox.appendChild(limitText);
  limitBox.appendChild(limitWarn);
  if (maxItems && maxItems !== 1 && layoutMode === "popup") wrap.appendChild(limitBox);

  function showLimitWarn(msg) {
    limitWarn.textContent = msg;
    limitWarn.style.display = 'block';
    clearTimeout(showLimitWarn._t);
    showLimitWarn._t = setTimeout(() => {
      limitWarn.style.display = 'none';
    }, 1600);
  }

  function updateLimitUI() {
    if (!(maxItems && maxItems !== 1)) return;
    limitText.textContent = `${cfg.selected_label || 'Valittu'}: ${selected.size} / ${maxItems}`;
  }

  // ---- Toggle all/none button ----
  let toggleBtn = null;
  if (!(maxItems === 1) && layoutMode === "popup") {
    toggleBtn = document.createElement('button');
    toggleBtn.style.cssText = `
      width:100%;
      padding:8px 10px;
      margin-bottom:10px;
      cursor:pointer;
      border-radius:6px;
      border:${cfg.btn?.border ? `${cfg.btn.border_width}px solid ${cfg.btn.border}` : 'none'};
    `;
    if (cfg.btn?.background) toggleBtn.style.background = cfg.btn.background;
    if (cfg.btn?.font?.size) toggleBtn.style.fontSize = cfg.btn.font.size + 'px';
    if (cfg.btn?.font?.color) toggleBtn.style.color = cfg.btn.font.color;
    if (cfg.btn?.font?.family) toggleBtn.style.fontFamily = cfg.btn.font.family;

    wrap.appendChild(toggleBtn);
  }

  // ---- Checkbox grid ----
  const grid = document.createElement('div');
  grid.style.cssText = 'display:flex; flex-wrap:wrap; gap:8px 16px; width:100%; align-items:flex-start;';
  wrap.appendChild(grid);

  // ---- Selection chip under modebar (popup only) ----
  let chip = null;
  let chipTextEl = null;
  let chipDismissed = false;
  let _lastChipKey = null;

  if (layoutMode === 'popup') {
    chip = document.createElement('div');
    chip.id = (cfg.id ? cfg.id + '-chip' : 'roboplot-chip');
    chip.style.cssText = `
      position:absolute;
      right:8px;
      z-index:9998;
      max-width:60%;
      display:none;
      align-items:center;
      gap:8px;
      padding:6px 10px;
      border-radius:8px;
      opacity:0.7;
      background:${cfg.btn?.background || '#fff'};
      border:${cfg.btn?.border ? `${cfg.btn.border_width}px solid ${cfg.btn.border}` : 'none'};
      box-shadow:0 4px 8px rgba(0,0,0,0.12);
      font-family:${cfg.btn?.font?.family || 'inherit'};
      color:${cfg.btn?.font?.color || 'inherit'};
      font-size:${cfg.btn?.font?.size || 12}px;
      line-height:1.2;
      user-select:none;
      white-space:nowrap;
      overflow:hidden;
      box-sizing:border-box;
    `;
    gd.appendChild(chip);

    chipTextEl = document.createElement('span');
    chipTextEl.style.cssText = `
      overflow:hidden;
      text-overflow:ellipsis;
      white-space:nowrap;
      cursor:pointer;
    `;
    chip.appendChild(chipTextEl);

    const chipClose = document.createElement('span');
    chipClose.innerHTML = cfg.close || '&times;';
    chipClose.style.cssText = `
      cursor:pointer;
      line-height:1;
      margin-left:2px;
      border-radius:50%;
      flex:0 0 auto;
    `;
    if (cfg.btn?.background) chipClose.style.background = cfg.btn.background;
    if (cfg.btn?.font?.size) chipClose.style.fontSize = cfg.btn.font.size + 'px';
    if (cfg.btn?.font?.color) chipClose.style.color = cfg.btn.font.color;
    if (cfg.btn?.font?.family) chipClose.style.fontFamily = cfg.btn.font.family;
    chip.appendChild(chipClose);

    chipTextEl.addEventListener('click', () => {
      wrap.style.display = 'block';
    });

    chipClose.addEventListener('click', (e) => {
      e.stopPropagation();
      chipDismissed = true;
      chip.style.display = 'none';
    });

    function positionChip() {
      const mb = gd.querySelector('.modebar');
      const mbH = mb ? mb.getBoundingClientRect().height : 0;
      chip.style.top = (mbH + 8) + 'px';
    }

    const relayoutHandler = () => positionChip();
    gd.on?.('plotly_relayout', relayoutHandler);
    gd._extmenu_plotlyRelayoutHandler = relayoutHandler;

    const windowResizeHandler = () => positionChip();
    window.addEventListener('resize', windowResizeHandler);
    gd._extmenu_windowResizeHandler = windowResizeHandler;

    function chipSummary() {
      if (selected.size === allF.length) return null;

      const vals = Array.from(selected);

      if (maxItems === 1) {
        return `${cfg.col}: ${vals[0] ?? ''}`.trim();
      }

      const shown = vals.slice(0, 3);
      const more = vals.length - shown.length;
      return `${cfg.col}: ${shown.join(', ')}${more > 0 ? ` (+${more})` : ''}`;
    }

    function updateChip(forceShow = false) {
      const txt = chipSummary();
      const key = Array.from(selected).sort().join('\u0001');
      const changed = (key !== _lastChipKey);
      _lastChipKey = key;

      if (changed) chipDismissed = false;

      if (!txt || (chipDismissed && !forceShow)) {
        chip.style.display = 'none';
        return;
      }

      chipTextEl.textContent = txt;
      chip.style.display = 'flex';
      positionChip();
    }

    gd._extmenu_updateChip = updateChip;
  } else {
    gd._extmenu_updateChip = function () {};
  }

  function updateToggleLabel() {
    if (!toggleBtn) return;

    if (!maxItems) {
      toggleBtn.textContent =
        (selected.size === allF.length)
          ? (cfg.deselect || 'Poista valinnat')
          : (cfg.select || 'Valitse kaikki');
      return;
    }

    if (selected.size === 0) {
      toggleBtn.textContent = cfg.select || `Valitse (${Math.min(maxItems, allF.length)})`;
    } else {
      toggleBtn.textContent = cfg.deselect || 'Poista valinnat';
    }
  }

  function inferCategoryAxis() {
    const bar = (gd.data || []).find(tr => tr.type === 'bar');
    if (!bar) return null;
    return (bar.orientation === 'h') ? 'yaxis' : 'xaxis';
  }

  function getCategoryOrderFromCfgOrOrig(axName) {
    if (cfg.categoryorder && Array.isArray(cfg.categoryorder) && cfg.categoryorder.length) {
      return cfg.categoryorder.map(v => '' + v);
    }

    const seen = new Set();
    const out = [];

    for (let ti = 0; ti < ORIG.length; ti++) {
      const o = ORIG[ti];
      const tr = gd.data[ti] || {};
      if (tr.type !== 'bar') continue;

      const axisCats = (tr.orientation === 'h') ? o.y : o.x;
      (axisCats || []).forEach(v => {
        const s = '' + v;
        if (!seen.has(s)) {
          seen.add(s);
          out.push(s);
        }
      });
    }

    return out;
  }

  function applyFilter() {
    const idxs = gd.data.map((_, i) => i);
    const update = { x: [], y: [] };

    const hasText = ORIG.some(o => o.text);
    const hasHover = ORIG.some(o => o.hovertext);
    if (hasText) update.text = [];
    if (hasHover) update.hovertext = [];

    for (let ti = 0; ti < ORIG.length; ti++) {
      const o = ORIG[ti];
      const keepIdx = [];

      for (let j = 0; j < o.filterMulti.length; j++) {
        const vs = o.filterMulti[j];
        if (vs.some(v => selected.has(v))) keepIdx.push(j);
      }

      update.x.push(keepIdx.map(j => o.x[j]));
      update.y.push(keepIdx.map(j => o.y[j]));
      if (hasText) update.text.push(o.text ? keepIdx.map(j => o.text[j]) : null);
      if (hasHover) update.hovertext.push(o.hovertext ? keepIdx.map(j => o.hovertext[j]) : null);
    }

    const ax = inferCategoryAxis();
    if (ax) {
      const cats = new Set();
      for (let i = 0; i < update.x.length; i++) {
        const tr = gd.data[i] || {};
        if (tr.type !== 'bar') continue;
        const axisCats = (tr.orientation === 'h') ? update.y[i] : update.x[i];
        (axisCats || []).forEach(v => cats.add(v));
      }

      const catsOrder = getCategoryOrderFromCfgOrOrig(ax);
      const visibleCats = catsOrder.filter(v => cats.has(v));
      const rel = {};
      rel[ax + '.categoryorder'] = 'array';
      rel[ax + '.categoryarray'] = visibleCats;
      Plotly.relayout(gd, rel);
    }

    Plotly.restyle(gd, update, idxs);
    Plotly.relayout(gd, {
      'xaxis.autorange': true,
      'yaxis.autorange': true
    });

    updateLimitUI();
    updateToggleLabel();
    gd._extmenu_updateChip();
  }

  function renderGrid(grid) {
    grid.innerHTML = '';

    const modeNow = effectiveLayoutMode();

    allF.forEach(v => {
      const isSelected = selected.has(v);

      const label = document.createElement('label');
      label.style.cssText = `
        display:flex;
        align-items:center;
        gap:${modeNow === "popup" ? 6 : 2}px;
        cursor:pointer;
        width:${modeNow === "side" ? '100%' : 'clamp(120px, 22vw, 240px)'};
        border-radius:6px;
        padding:${modeNow === "popup" ? 4 : 1}px ${modeNow === "popup" ? 6 : 2}px;
        transition:opacity 0.15s ease, filter 0.15s ease, background 0.15s ease;
        ${noCheckmarks && !isSelected ? 'opacity:0.45; filter:grayscale(1);' : 'opacity:1; filter:none;'}
        box-sizing:border-box;
      `;

      function applySelectionChange(nextChecked) {
        if (maxItems === 1) {
          if (nextChecked) {
            selected.clear();
            selected.add(v);
          } else {
            selected.delete(v);
          }
          renderGrid(grid);
          applyFilter();
          return;
        }

        if (nextChecked) {
          if (maxItems && selected.size >= maxItems) {
            showLimitWarn(cfg.limit_reached || `Enintään ${maxItems} valintaa`);
            return;
          }
          selected.add(v);
        } else {
          selected.delete(v);
        }

        if (noCheckmarks) renderGrid(grid);
        applyFilter();
      }

      if (!noCheckmarks) {
        const cb = document.createElement('input');
        cb.type = 'checkbox';
        cb.checked = isSelected;
        cb.style.accentColor = cfg.checkmark?.color || '#666';
        cb.style.transform = 'scale(' + (cfg.checkmark?.size || 1) + ')';

        cb.addEventListener('change', () => {
          applySelectionChange(cb.checked);
        });

        label.appendChild(cb);
      } else {
        label.addEventListener('click', (e) => {
          e.preventDefault();
          applySelectionChange(!isSelected);
        });
      }

      const span = document.createElement('span');
      span.textContent = v;
      span.style.cssText = `
        ${noCheckmarks && !isSelected ? `color:${cfg.box?.font?.color || 'inherit'};` : ''}
        overflow:hidden;
        text-overflow:ellipsis;
      `;

      label.appendChild(span);
      grid.appendChild(label);
    });

    if (cfg.title === true) {
      const titleEl = document.createElement('div');
      titleEl.textContent = cfg.col;
      titleEl.style.cssText = `
        width:100%;
        text-align:left;
        font-weight:bold;
        margin-bottom:${cfg.btn?.font?.size ? Math.round(cfg.btn.font.size / 2) + 'px' : '5px'};
        font-size:${cfg.btn?.font?.size ? (cfg.btn.font.size + 2) + 'px' : 'inherit'};
      `;
      grid.prepend(titleEl);
    }

    updateLimitUI();
    updateToggleLabel();
  }

  if (toggleBtn) {
    toggleBtn.addEventListener('click', () => {
      if (!maxItems) {
        if (selected.size === allF.length) selected.clear();
        else allF.forEach(v => selected.add(v));
      } else {
        if (selected.size > 0) {
          selected.clear();
        } else {
          const out = [];
          const seen = new Set();
          for (let ti = 0; ti < ORIG.length && out.length < maxItems; ti++) {
            const valsThisTrace = uniq(ORIG[ti].filterMulti.flat());
            for (const v of valsThisTrace) {
              if (seen.has(v)) continue;
              out.push(v);
              seen.add(v);
              if (out.length >= maxItems) break;
            }
          }

          if (out.length === 0) {
            allF.slice(0, Math.min(maxItems, allF.length)).forEach(v => selected.add(v));
          } else {
            out.forEach(v => selected.add(v));
          }
        }
      }

      renderGrid(grid);
      applyFilter();
    });
  }

  // ---- Observe size changes ----
  gd._extmenu_resizeObserver = new ResizeObserver(() => {
    if (resizeRaf) cancelAnimationFrame(resizeRaf);
    resizeRaf = requestAnimationFrame(() => {
      resizeRaf = null;
      delete gd._extmenu_resizeRaf;
      const modeChanged = applyMenuLayout();
      if (modeChanged && effectiveLayoutMode() !== 'popup') renderGrid(grid);
    });
    gd._extmenu_resizeRaf = resizeRaf;
  });
  gd._extmenu_resizeObserver.observe(layoutHost.parentElement || layoutHost);

  renderGrid(grid);
  applyFilter();
}

function rangeSliderShowHide(el, show = true) {
  if ('rangeslider' in el.layout.xaxis) {
    if(el.layout.xaxis.rangeslider.visible != show) {
      Plotly.relayout(el, {"showlegend" : show, "xaxis.rangeslider.visible": show});
    } else {
      Plotly.relayout(el, {"showlegend" : show});
    }
  } else {
    Plotly.relayout(el, {"showlegend" : show});
  }

}

function rangeSliderHeight(el) {
  let elslider = 0;
  if ('rangeslider' in el.layout.xaxis) {
    if(el.layout.xaxis.rangeslider.visible == true) {
      elslider_container = $(el).find('.rangeslider-bg')[0].getBBox().height
      elslider = elslider_container+(el.layout.margin.t*0.1)+(el.layout.margin.b*0.1)
      let xticks = $(el).find('g.xaxislayer-above')
      if(xticks.length > 0) {
        xticks = $(el).find('g.xaxislayer-above')[0].getBBox().height
        elslider = elslider + (xticks / 10)

      }
    }
  }

  return (elslider)
}

function findModeBarHeight(el) {
  let modebar_ht = 5;
  let elmodebar = $(el).find('div.modebar');
  if(elmodebar.length > 0) {
    if(elmodebar.css("display") != "none") {
      modebar_ht = elmodebar[0].clientHeight;
    }
  }
  return modebar_ht
}

function getVerticalLayout(el, legend_fontsize, height = false, keys, pie_chart, logo = undefined, tidy_legend = false, legend_position = "auto") {
  //  console.log("NEW RELAYOUT")
  //  console.log("margin b init: " + el.layout.margin.b)
  const checkForBR = (string) => (/<br>/.test(string)) ? 11 : 16;
  adjustLegendItems(el, tidy_legend);
  let elcontainer = {height: $(el).find("svg.main-svg")[0].height.animVal.value, width: $(el).find("svg.main-svg")[0].width.animVal.value};
  let eltitle = $(el).find('g.g-gtitle')[0].getBBox();
  let is_yaxis2 = $(el).find('g.xy2').length > 0 ? true: false
  let is_yaxistitle2 = $(el).find('g.g-y2title').length > 0 ? true: false
  let elslider = rangeSliderHeight(el)
  let elxticks_default = pie_chart ? 5 : 0
  let elxticks = $(el).find('g.xaxislayer-above')
  if(elxticks.length > 0) { elxticks = elxticks[0].getBBox().height } else { elxticks =  elxticks_default };
  //console.log("xticks: "+ elxticks)
  let modebar_ht = findModeBarHeight(el)
  let margin_right = 5
  if(is_yaxis2) {
    let yaxis2 = $(el).find('g.overaxes-above > g.xy2-y')[0].getBBox();
    let yaxiswidth = is_yaxistitle2 ? $(el).find('g.g-y2title')[0].getBBox().width : 0
    margin_right = yaxis2.width+5+yaxiswidth
  }
  let margin_top = eltitle.height + modebar_ht + checkForBR(el.layout.title.text);
  let elcaption = $(el).find('g.annotation')[0].getBBox().height + 5;
  let elxtitle = $(el).find('g.g-xtitle')
  if(elxtitle.length > 0) {
    elxtitle = elxtitle[0].getBBox().height * 1.3;
  } else {
    elxtitle = 0
  }
  let ellegend = {width: 0, height: 0}
  if ($(el).find('g.legend')[0] != undefined) {
    let legendbox = $(el).find('g.legend')[0].getBBox()
    ellegend.height = legendbox.height//legend_position == "right" ? 0 : Math.ceil(legendbox.height)
    ellegend.width =  legendbox.width
  };
  let elplot = pie_chart ? $(el).find('.pielayer') : $(el).find('.nsewdrag');
  if (elplot.length > 0) {elplot = elplot[0].getBBox()};
  // console.log("margin b: " + margin_bottom)
  if(legend_position == "auto") {
    if(el.layout.legend.position == "right") {
      legend_position = elplot.width / elcontainer.width < 0.67 || elplot.width / ellegend.width < 0.75 ? "bottom" : "right"
    } else {
      legend_position = elplot.width / elcontainer.width > 0.75 || elplot.width / ellegend.width > 1.2 ? "right" : "bottom"
    }
  }
  let legend_orientation = legend_position == "bottom" ? "h" : "v"
  if ($(el).find('g.legend')[0] != undefined) {
    let legendbox = $(el).find('g.legend')[0].getBBox()
    ellegend.height = legend_position == "right" ? 0 : Math.ceil(legendbox.height)
    ellegend.width =  legendbox.width
  };
  el.layout.legend.position = legend_position;
  let legend_x = legend_position == "bottom" ? 0 : 1.02
  let margin_bottom = ellegend.height + 15 + (elcaption + elxticks + elxtitle);
  if (pie_chart) { elplot.height = elcontainer.height-margin_bottom-margin_top }
  if (elplot.height == 0) { elplot.height = 1}
  let elimages = $(el).find('g.layer-above > g.imagelayer > image')[0].getBBox();
  let logospace = logoSpace(logo, elimages, margin_bottom, elxtitle, elxticks, ellegend, legend_orientation, elplot.height);
  margin_bottom = margin_bottom + logospace;
//  console.log((legend_fontsize.legend * 2) / elplot.height)
//  let images_sizey = (elcontainer.height * 0.05) / elplot.height;
  let images_sizey = ((legend_fontsize.legend * 2) / elplot.height)
  el.layout.images[0].sizey = images_sizey
  let legend_y = legend_position == "right" ? 1 : -((elxticks + 10 + (elslider) + elxtitle) / elplot.height)//((margin_bottom - elcaption + (elslider*2)) / elplot);
  if (legend_y < -2 || (elplot.height < (elcontainer.height / 4))) {
    rangeSliderShowHide(el, false)
    elslider = 0;
    elxticks = $(el).find('g.xaxislayer-above')
    if(elxticks.length > 0) { elxticks = elxticks[0].getBBox().height } else { elxticks = elxticks_default };
    margin_bottom = 15 + elcaption + elxticks + elxtitle;
    let elplot = pie_chart ? $(el).find('.pielayer') : $(el).find('.nsewdrag');
    if (elplot.length > 0) {
      elplot = elplot[0].getBBox()

    };
    if (pie_chart) { elplot.height = elcontainer.height-margin_bottom-margin_top }
    if (elplot.height == 0) { elplot.height = 1}
    //images_sizey = (elcontainer.height * 0.05) / elplot.height;
    images_sizey = ((legend_fontsize.legend * 2) / elplot.height)
    el.layout.images[0].sizey = images_sizey
    legend_y = legend_position == "right" ? 1 : -((elxticks + 10 + (elslider) + elxtitle) / elplot.height)//((margin_bottom - elcaption + (elslider*2)) / elplot);

  }  else if ((elplot.height > (elcontainer.height / 1.5)) & el.layout.showlegend == false) {
    //    console.log("NUFF SPACE")
    rangeSliderShowHide(el, true)
    elslider = rangeSliderHeight(el)
    let elxticks = $(el).find('g.xaxislayer-above')
    if(elxticks.length > 0) { elxticks = elxticks[0].getBBox().height } else { elxticks =  elxticks_default };
    let modebar_ht = findModeBarHeight(el)
    let margin_top = eltitle.height + modebar_ht + checkForBR(el.layout.title.text);
    let elcaption = $(el).find('g.annotation')[0].getBBox().height + 5;
    let elxtitle = $(el).find('g.g-xtitle')
    if(elxtitle.length > 0) {
      elxtitle = elxtitle[0].getBBox().height * 1.3;
    } else {
      elxtitle = 0
    }
    ellegend = {width: 0, height: 0}
    if ($(el).find('g.legend')[0] != undefined) {
      ellegend.height = legend_position == "bottom" ? Math.ceil($(el).find('g.legend')[0].getBBox().height) : 0
      ellegend.width =  $(el).find('g.legend')[0].getBBox().width
    };
    margin_bottom = ellegend.height + 15 + (elcaption + elxticks + elxtitle);
    //  margin_bottom = margin_bottom + logoSpace(logo, elimages, margin_bottom, elxtitle, elxticks, ellegend);
    //console.log("margin b loosen: " + el.layout.margin.b)
    elplot = pie_chart ? $(el).find('.pielayer') : $(el).find('.nsewdrag');
    if (elplot.length > 0) {elplot = elplot[0].getBBox()};
    if (pie_chart) { elplot.height = elcontainer.height-margin_bottom-margin_top }
    if (elplot.height == 0) { elplot.height = 1}
    elimages = $(el).find('g.layer-above > g.imagelayer > image')[0].getBBox();
    //images_sizey = (elcontainer.height * 0.05) / elplot.height;
    images_sizey = ((legend_fontsize.legend * 2) / elplot.height)
    el.layout.images[0].sizey = images_sizey
    legend_y = -((elxticks + 10 + (elslider) + elxtitle) / elplot.height)//((margin_bottom - elcaption + (elslider*2)) / elplot);
  }
  elplot = pie_chart ? $(el).find('.pielayer') : $(el).find('.nsewdrag');
  if (elplot.length > 0) { elplot = elplot[0].getBBox() };
  if (pie_chart) { elplot.height = elcontainer.height-margin_bottom-margin_top }
  if (elplot.height == 0) { elplot.height = 1}
  let legend_font_size = (ellegend.height > (elplot.height / 2)) ? legend_fontsize.legend - 2 : legend_fontsize.legend;
  legend_font_size = (ellegend.width > $(el).find("svg.main-svg")[0].width.animVal.value) ? legend_font_size - 2 : legend_font_size;
  let yaxis_font_size = legend_fontsize.y;

  let yaxis_layer = $(el).find('g.yaxislayer-above')

  if(yaxis_layer.length > 0) {
    let yticks = $(el).find('g.ytick');
    let yvertical = [...yticks].reduce((a,b) => a + b.getBBox().height, 0)
    if( elplot.height <= yvertical) {
      yaxis_font_size = Math.floor(el.layout.yaxis.tickfont.size * 0.8)
    } else {
      yaxis_font_size = Math.min(Math.floor(el.layout.yaxis.tickfont.size*1.5), legend_fontsize.y)
    }

    let yaxis_width = yaxis_layer[0].getBBox().width;

    if(el.layout.annotations[0].xmod == "container") {
      el.layout.annotations[0].xshift = -yaxis_width;
    }

    if(el.layout.legend.xmod == "container" && legend_position == "bottom") {
      legend_x = -(yaxis_width / elplot.width)
    }

  }
  
  let xaxis_range = findXaxisRangeY(el, elplot, pie_chart);
  
  let thearray = {
    'legend.orientation': legend_orientation,
    'legend.x': legend_x,
    'legend.y': legend_y,
    'images[0].sizey': images_sizey,
    'margin.t': margin_top,
    'margin.b': margin_bottom,
    'margin.r': margin_right,
    'legend.font.size': legend_font_size,
    'yaxis.tickfont.size': yaxis_font_size,
    'yaxis2.tickfont.size': yaxis_font_size
  };

  if(xaxis_range !== undefined) {
    thearray["xaxis.range[0]"] = xaxis_range[0];
    thearray["xaxis.range[1]"] = xaxis_range[1];
    
  }
  
  let rearray = keys.reduce(function (obj2, key) {
    if (key in thearray) // line can be removed to make it inclusive
    obj2[key] = thearray[key];
    return obj2; 

  }, {});

/*  if(pie_chart) {
    const keyToRemove = 'b';
    const filtered = Object.fromEntries(
  Object.entries(rearray).filter(([key]) => key !== keyToRemove)
);
    rearray = filtered
  }*/

  return rearray;

}


  findXaxisRangeY = function(el, elplot, pie) {
    
    if(pie === true) {
      return (undefined)
    }
  
    let is_numeric = el.data[0].x.every(v => typeof v === "number");
    
    if(!is_numeric) {
      return(undefined)
    }
  
    let pts = $(el).find('.point')
    pts = Array.from(pts)
    if(pts.length == 0) {
      return(el._init_xrange.x1)
    }
    let highs = [];
    let lows = [] 
    pts = pts.forEach(pt => {
      if(pt.__data__.x >= 0) {
          highs.push(pt.getBBox().width) 
      } else {
        lows.push(pt.getBBox().width)
      }
    })
    

    if(lows.length > 0 && highs.length > 0) {

      highs = Math.max(...highs)
      lows = Math.max(...lows)
      let lows_portion = lows / (lows+highs)
      let highs_portion = highs / (lows+highs)
/*
      console.log("highs + lows " + (highs+lows))
      console.log("diff: " + Math.abs(elplot.width - (highs+lows)))
      console.log("total: " + elplot.width)
*/
      let multiplier = (Math.abs(((elplot.width*0.95) - (highs+lows)) / (elplot.width)*0.95))
      if((highs + lows) > elplot.width * 0.95) {
//        console.log("space needed, multiplier is " + multiplier)
        let lobound = 1 + (lows_portion * multiplier)
        let hibound = 1 + (highs_portion  * multiplier)
//        console.log("lows portion is " + lows_portion + ", highs portion is " + highs_portion)
        lobound = Math.abs(el.layout.xaxis.range[0]) * lobound
        hibound = el.layout.xaxis.range[1] * hibound
        let res = [-lobound-elplot.width*0.02, hibound]
        return(res)
        
      } else if((highs + lows) < (elplot.width * 0.9)) {
//        console.log("nuff space, multiplier is " + multiplier)
        let lobound = 1+(lows_portion * multiplier)
        let hibound = 1+(highs_portion * multiplier)
        lobound = Math.abs(el.layout.xaxis.range[0]) * lobound
        lobound = Math.min(lobound, Math.abs(el._init_xrange.x0))
        hibound = el.layout.xaxis.range[1] * hibound
        hibound = Math.min(hibound, el._init_xrange.x1)
        res = [-lobound, hibound]
        return(res)
        
      } else {
//        console.log("naah")
        return(undefined)
      }
      
    }
    
    if(highs.length > 0) {
      highs = Math.max(...highs)
      let multiplier
    if(highs > elplot.width) {
         multiplier = 1+Math.abs((elplot.width - highs) / elplot.width) 
         return([el._init_xrange.x0, el.layout.xaxis.range[1] * multiplier])
    } else if (highs < elplot.width * 0.9) {
      multiplier = Math.abs(Math.abs((elplot.width - highs) / elplot.width)-1 )
      return([el._init_xrange.x0, Math.min(el.layout.xaxis.range[1] / multiplier, el._init_xrange.x1)])
  }
  }
    
    if(lows.length > 0) {
      lows = Math.max(...lows)
      let multiplier
    if(lows > elplot.width) {
         multiplier = 1+Math.abs((elplot.width - lows) / elplot.width) 
         return([-(Math.abs(el.layout.xaxis.range[0]) * multiplier), el._init_xrange.x1])
    } else if (lows < elplot.width * 0.9) {
      console.log("nuff space")
      multiplier = Math.abs(Math.abs((elplot.width - lows) / elplot.width)-1 )
      console.log(multiplier)
      return([-Math.min(Math.abs(el.layout.xaxis.range[0]) / multiplier, Math.abs(el._init_xrange.x0)), el._init_xrange.x1])
  }
  }
  
  return(undefined)
  
  }
  
function calculateDisplayedImageSize(imageAspectRatio, container) {
  let containerAspectRatio = container.width / container.height;
  let displayedWidth, displayedHeight;
  if (containerAspectRatio > imageAspectRatio) {

    displayedHeight = container.height;
    displayedWidth = displayedHeight * imageAspectRatio;
  } else {

    displayedWidth = container.width;
    displayedHeight = displayedWidth / imageAspectRatio;
  }

  let result = { width: Math.round(displayedWidth), height: Math.round(displayedHeight) }

  return result;
}

function logoSpace(ratio, image, margin_b, title_h, xtick_h, legend, legend_orientation, limit) {
  let logo_actual = calculateDisplayedImageSize(ratio, image)
  let logo_space = [];
  let lwidth = legend_orientation == "bottom" ? legend.width : 0
  logo_space.horizontal = (image.width - legend.width) - logo_actual.width;
  logo_space.vertical = margin_b - (title_h + xtick_h + legend.height) - logo_actual.height
  if (Object.values(logo_space).every(x => x < 0)) {
    if(-logo_space.vertical > (limit/3)) {
      return 0
    } else {
      return -logo_space.vertical
    }
  } else {
    return 0
  }
}

function adjustLegendItems(gd, tidy = true) {

  let legendItemTexts = $(gd).find('.legend .legendtext')

  if(tidy) {
    let maxWidth = gd.data.reduce((max, trace) => {
      let thisLegendItem = legendItemTexts.filter(function() {
        return $(this).text() === trace.name;
      });
      if (thisLegendItem.length > 0) {
        let thisLegendWidth = thisLegendItem[0].getBBox().width;
        return Math.max(max, thisLegendWidth);
      }
      return max;
    }, 0);

    maxWidth = Math.round(maxWidth * 1.02)
    gd.layout.legend.entrywidth = maxWidth;
  } else {
    let legendItemTexts = $(gd).find('.legend .legendtext')

    gd.data.forEach(trace => {
      let thisLegendItem = legendItemTexts.filter(function() {
        return $(this).text() === trace.name;
      });
      if(thisLegendItem.length > 0) {
        let thisLegendWidth = thisLegendItem[0].getBBox().width
        trace.legendwidth = Math.round(thisLegendWidth*1.1)
      }
    })
  }


}



function setVerticalLayout(eventdata, gd, legend_fontsize, plot_title, pie_chart, logo = undefined, tidy_legend = false, legend_position = "auto") {
  if ('width' in eventdata | 'autosize' in eventdata) {
    if ('rangeslider' in gd.layout.xaxis) {
      if (gd.layout.xaxis.rangeslider.visible == false) {
        gd.layout.xaxis.rangeslider.visible = true;
      }}
    let title_text = "<span>" +
      (plot_title[2] ? "<b>" : "" ) +
      plot_title[0] +
      (plot_title[2] ? "</b>" : "" ) +
      (plot_title[0].length == 0 ? "" : "<br>") +
      "<span style='font-size: 75%'>" + plot_title[1] + "</span></span>"
    let caption_text = gd.layout.annotations[0].text.replace(new RegExp("<br class = 'roboplotr-breaker'>", 'g'), " ")
    Plotly.relayout(gd, {'title.text': title_text, 'annotations[0].text': caption_text})
    let gdtitle = $(gd).find('g.g-gtitle')[0].getBBox().width;
    let titlespace = pie_chart ? $(gd).find('g.layer-above') : $(gd).find('.nsewdrag');
    if (titlespace.length > 0) {titlespace = titlespace[0].getBBox().width};
    let relayout_array = getVerticalLayout(gd, legend_fontsize, false, keys = ['legend.font.size','margin.t','margin.b','legend.orientation', 'legend.x','legend.y','yaxis.tickfont.size','yaxis2.tickfont.size'], pie_chart = pie_chart, logo = logo, tidy_legend = tidy_legend, legend_position = legend_position)
    if(titlespace <= gdtitle) {
      title_text = "<span>" +
        (plot_title[2] ? "<b>" : "" ) +
        stringDivider(plot_title[0], Math.floor(titlespace/(gd.layout.title.font.size-8)), "<br class = 'roboplotr-breaker'>") +
        (plot_title[2] ? "</b>" : "" ) +
        "<br><span style='font-size: 75%'>" + plot_title[1] + "</span></span>"
      relayout_array["title.text"] = title_text;
    }
    rangeSliderShowHide(gd, true);
    Plotly.relayout(gd, relayout_array);
    let logo_width = calculateDisplayedImageSize(logo, $(gd).find('g.layer-above > g.imagelayer > image')[0].getBBox()).width
    relayout_array = getVerticalLayout(gd, legend_fontsize, false, keys = ['legend.font.size','legend.orientation','legend.x','legend.y','margin.t','margin.b','margin.r','yaxis.tickfont.size','yaxis2.tickfont.size','images[0].sizey', 'xaxis.range[0]', 'xaxis.range[1]'], pie_chart = pie_chart, logo = logo, tidy_legend = tidy_legend, legend_position = legend_position)
    relayout_array = findCaptionSpace(gd, logo, pie_chart, relayout_array, titlespace);
    Plotly.relayout(gd, relayout_array);
    relayout_array = getVerticalLayout(gd, legend_fontsize, false, keys = ['legend.font.size','legend.orientation','legend.x','legend.y','margin.t','margin.b','margin.r','yaxis.tickfont.size','yaxis2.tickfont.size','images[0].sizey', 'xaxis.range[0]', 'xaxis.range[1]'], pie_chart = pie_chart, logo = logo, tidy_legend = tidy_legend, legend_position = legend_position)
    relayout_array = findCaptionSpace(gd, logo, pie_chart, relayout_array, titlespace);
    Plotly.relayout(gd, relayout_array);
    relayout_array = getVerticalLayout(gd, legend_fontsize, false, keys = ['legend.font.size', 'margin.t', 'margin.b','legend.orientation','legend.x','legend.y','images[0].sizey','yaxis.tickfont.size','yaxis2.tickfont.size','xaxis.range[0]', 'xaxis.range[1]'], pie_chart = pie_chart, logo = logo, tidy_legend = tidy_legend, legend_position = legend_position);
    relayout_array = findCaptionSpace(gd, logo, pie_chart, relayout_array, titlespace);
    Plotly.relayout(gd, relayout_array);
    relayout_array = getVerticalLayout(gd, legend_fontsize, false, keys = ['legend.font.size', 'margin.t', 'margin.b','legend.orientation','legend.x','legend.y','images[0].sizey','yaxis.tickfont.size','yaxis2.tickfont.size','xaxis.range[0]', 'xaxis.range[1]'], pie_chart = pie_chart, logo = logo, tidy_legend = tidy_legend, legend_position = legend_position);
    Plotly.relayout(gd, relayout_array);
    setUpdatemenuPosition(gd);

  }
}

function setUpdatemenuPosition(gd) {
    if($(gd).find("g.updatemenu-container").length > 0) {
      let plot = $(gd).find('.nsewdrag')[0].getBBox();
      let updatemenu = $(gd).find("g.updatemenu-container")[0].getBBox();
      let updatemenu_y = gd.layout.updatemenus[0].yanchor == "top" ? 0.98 : 0.02;
//      console.log(gd.layout.updatemenus[0].xanchor)
//      console.log(plot.width)
      let updatemenu_x = gd.layout.updatemenus[0].xanchor == "left" ? 0.02 : 0.98;
//      let updatemenu_x = 0.9
      Plotly.relayout(gd, {
        "updatemenus[0].y": updatemenu_y,
        "updatemenus[0].x": updatemenu_x
      });
    }
}

function setYPositions(eventdata, gd, pie_chart = false) {

  if ('width' in eventdata | 'autosize' in eventdata) {

    let container = $(gd).find("svg.main-svg")[0].height.animVal.value;
    let modebar_ht = findModeBarHeight(gd)
    let title_y = (container - (21+modebar_ht)) / container
    let plot = pie_chart ? $(gd).find('.pielayer') : $(gd).find('.nsewdrag');
    if (plot.length > 0) {plot = plot[0].getBBox()};
    // console.log("margin b: " + margin_bottom)
    let margin_bottom = gd.layout.margin.b
    let margin_top = gd.layout.margin.t
     if (pie_chart) { plot.height = container-margin_bottom-margin_top }
    let mb = container-plot.height-gd.layout.margin.t
    let ph = Math.round(container-margin_top-margin_bottom)
    if(pie_chart === true) {
      let container_width = $(gd).find("svg.main-svg")[0].width.animVal.value;
      if(ph > Math.round(plot.height) && container > container_width*2) {
        mb = mb - (ph-plot.height)
        plot.height = plot.height + (ph-plot.height)
      }
    }

    let low_bound = -(mb / plot.height)
    let annotations_y = low_bound
    let images_y = low_bound

    //  console.log("low bound: " + low_bound)
    Plotly.relayout(gd, {"images[0].y": low_bound, "annotations[0].y": low_bound, "title.y": title_y})

  }

}

function findCaptionSpace(gd, logo, pie_chart, relayout_array, titlespace) {
  let captionspace
  let gdcaption = $(gd).find('g.annotation')[0].getBBox().width;
  let caption_text = gd.layout.annotations[0].text.replace(new RegExp("<br class = 'roboplotr-breaker'>", 'g'), " ")
  if(pie_chart) {
    captionspace = titlespace
  } else {
    let plot_width = $(gd).find('.nsewdrag')[0].getBBox().width
    let logo_width = calculateDisplayedImageSize(logo, $(gd).find('g.layer-above > g.imagelayer > image')[0].getBBox()).width
    captionspace = Math.max(plot_width - logo_width, Math.round(logo_width / 2))
  }
  relayout_array['annotations[0].text'] = stringDivider(caption_text, Math.floor(captionspace/(gd.layout.annotations[0].font.size/2)), "<br class = 'roboplotr-breaker'>")

  return relayout_array
}

function findAnnotationById(plotDiv, customId) {
  let annotations = plotDiv.layout.annotations || [];
  for (let i = 0; i < annotations.length; i++) {
    if (annotations[i].annotationId === customId) {
      return i;
    }
  }
  return null; // If no shape is found with the given identifier
}


function findShapeById(plotDiv, customId) {
  let shapes = plotDiv.layout.shapes || [];
  for (let i = 0; i < shapes.length; i++) {
    if (shapes[i].shapeId === customId) {
      return i;
    }
  }
  return null; // If no shape is found with the given identifier
}

function editShapes(gd, line_label) {
  let sareashape = findShapeById(gd, "shadearea")
  if(sareashape !== null) {
    gd.layout.shapes[sareashape].y0 = gd.layout.yaxis.range[0]
    gd.layout.shapes[sareashape].y1 = gd.layout.yaxis.range[1]
    if(gd.layout.shapes[sareashape].xnull == true) {
      gd.layout.shapes[sareashape].x1 = gd.layout.xaxis.range[1]
    }
  }
  let zlineshape = findShapeById(gd, 'zeroline');
  if (zlineshape !== null) {
    gd.layout.shapes[zlineshape].x0 = gd.layout.xaxis.range[0]
    gd.layout.shapes[zlineshape].x1 = gd.layout.xaxis.range[1]
  }
  if(zlineshape !== null | sareashape !== null) {
    Plotly.redraw(gd)
  }
  if(zlineshape !== null) {
    gd.on('plotly_afterplot', function() {
      let yticks = $(gd).find('g.ytick text')
      let zeroline = $(gd).find('path.zl')
      if(line_label == 0) {
        if (zeroline.length > 0) {
          zeroline[0].style['stroke'] = gd.layout.shapes[zlineshape].line.color
        }
      } else {
        let label_translate
        if (zeroline.length > 0) { zeroline[0].style['stroke'] = gd.layout.yaxis.gridcolor };
        yticks.filter(function(d, i) {
          if(this.textContent.trim().replace(',','.').replace('\u2212','-').replace(' ','') == line_label) {
            label_translate = i.getAttribute('transform')
            let lines = $(gd).find('path.ygrid')
            lines.filter(function(d, i) {
              if(i.getAttribute('transform') == label_translate) {
                i.style['stroke'] = gd.layout.shapes[zlineshape].line.color
              }
            })
          }
        })
      }
    })
  }
}

function stringDivider(str, width, spaceReplacer) {
  const lines = str.split('<br>');

  const divideLine = (line) => {
    if (line.length > width) {
      let p = width;
      while (p > 0 && line[p] !== ' ') {
        p--;
      }
      if (p > 0) {
        const left = line.substring(0, p);
        const right = line.substring(p + 1);
        return left + spaceReplacer + divideLine(right);
      } else {
        const sp = /( ){1,}/ig;
        return line.replace(sp, spaceReplacer);
      }
    }
    return line;
  };

  const dividedLines = lines.map(divideLine);
  return dividedLines.join('<br>');
}


function estimateLeftMargin(labels, fontSize = 12) {
  const longest = Math.max(...labels.map(s => String(s ?? '').length));
  return Math.ceil(40 + longest * fontSize * 0.6);
}

function yrangeRelayout(eventdata, gd, timerId, trace_sums) {
  
  if (gd.data[0].x.every(v => typeof v !== "number") && Object.prototype.toString.call(eventdata['xaxis.range']) === '[object Array]' | 'xaxis.range[0]' in eventdata | "xaxis.autorange" in eventdata) {
    var xRange = gd.layout.xaxis.range;
    var yRange = gd.layout.yaxis.range;
    var yInside = [];
    let yStackInside = [];
    var traceTypes = [];
    //		var xInside = [];
    var visdata = gd.data.filter(trace => trace.visible === true || !(trace.hasOwnProperty('visible')));
    visdata.forEach(trace => {
      var len = trace.y.length;//Math.min(trace.x.length, trace.y.length);
      traceTypes.push(trace.type);
      //			console.log(trace.type)
      for (var i = 0; i < len; i++) {
        var x = trace.x[i];
        var y = trace.y[i];
        if ((x >= xRange[0] && x <= xRange[1]) || gd.layout.xaxis.type == "category") {
          //					xInside.push(x);
          yInside.push(y);
          if(trace.type == "bar") {
            let obj = {date: x, val: y}
            yStackInside.push(obj)
          }
        }
      }
    });

    if(trace_sums == true) {
      var holdermax = {};
      yStackInside.forEach(function(d) {
        if (holdermax.hasOwnProperty(d.date) && d.val > 0) {
          holdermax[d.date] = holdermax[d.date] + d.val;
        } else if (d.val > 0) {
          holdermax[d.date] = d.val;
        }
      });

      var holdermin = {};
      yStackInside.forEach(function(d) {
        if (holdermin.hasOwnProperty(d.date) && d.val <= 0) {
          holdermin[d.date] = holdermin[d.date] + d.val;
        }  else if (d.val <= 0) {
          holdermin[d.date] = d.val;
        }
      });

      yStackInside = [{val: 0}];
      for (var prop in holdermin) {
        yStackInside.push({ date: prop, val: holdermin[prop] });
      }
      for (var prop in holdermax) {
        yStackInside.push({ date: prop, val: holdermax[prop] });
      }

      yInside = yInside.concat(yStackInside.map(o => o.val));
    }
    let yMax
    let yMin
    if("xaxis.autorange" in eventdata) {
      yMax = gd._init_yrange.x1;
      yMin = gd._init_yrange.x0;
    } else {
    yMax = Math.max(...yInside);
    yMin = Math.min(...yInside);
    let yMod = Math.abs(yMax-yMin)*0.04
    yMax = yMax + yMod;
    let barpos = traceTypes.includes("bar") && yInside.every(y => y >= 0)
    if(trace_sums != true && barpos == false) {
         yMin = yMin - yMod;
    }
    }
    let zline = findShapeById(gd, "zeroline")
    let sarea = findShapeById(gd, "shadearea")
    var update = {'yaxis.range': [yMin,yMax]}
    if(zline != null) {
      if("xaxis.autorange" in eventdata) {
          update['shapes['+zline+'].x0'] = gd._init_xrange.x0
          update['shapes['+zline+'].x1'] = gd._init_xrange.x1
        } else {
          update['shapes['+zline+'].x0'] = xRange[0]
          update['shapes['+zline+'].x1'] = xRange[1]
        }
    }

    if(sarea != null) {
      if(gd.layout.shapes[sarea].xnull == true) {
        if("xaxis.autorange" in eventdata) {
          update['shapes['+sarea+'].x1'] = gd._init_xrange.x1
        } else {
          update['shapes['+sarea+'].x1'] = xRange[1]
        }
      }

      update['shapes['+sarea+'].y0'] = yMin
      update['shapes['+sarea+'].y1'] = yMax
    }
    Plotly.relayout(gd, update);
    if (timerId >= 0) {
      //timer is running: stop it
      window.clearTimeout(timerId);
    }
    timerId = window.setTimeout(function() {
      //fire end event
      //console.log('rangeslider event ENDS');
      //reset timer to undefined
      timerId = -1;
    }, 800);
  }
}

function plotlyRelayoutEventFunction(eventdata, gd, legend_fontsize, plot_title, rangeslider_sums, pie_chart, logo, tidy_legend, legend_position) {
  timerId = 0;
  //  gd.layout.margin.b = 0
  //  Plotly.redraw(gd)
  setVerticalLayout(eventdata, gd, legend_fontsize, plot_title, pie_chart, logo, tidy_legend, legend_position);
  setYPositions(eventdata, gd, pie_chart);
  yrangeRelayout(eventdata, gd, timerId, rangeslider_sums);

  //	console.log("margin b: " + gd.layout.margin.b, "; margin t: " + gd.layout.margin.t)
};
