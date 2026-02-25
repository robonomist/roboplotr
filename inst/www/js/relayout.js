function setExternalMenu(el, x) {
  const gd = el;
  const cfg = x.roboplot_externalmenu;
  if (!cfg || !cfg.col) return;

  const uniq = arr => Array.from(new Set(arr));
  const asArr = v => Array.isArray(v) ? v : (v == null ? [] : [v]);

  const maxItems = cfg["max-items"] ?? null; // validated in R

  // ---- Normalize a single per-point filter value to scalar OR multiple scalars ----
  function normVal(v) {
    if (v == null) return [];
    if (Array.isArray(v)) return v.filter(x => x != null).map(x => '' + x);
    return ['' + v];
  }

  // ---- Capture original per-trace arrays ----
  // redo here
  const original = (gd.data || []).map(tr => {
  const full = (tr && tr.meta && tr.meta.extmenu_full) ? tr.meta.extmenu_full : tr;

  const xvals = asArr(full.x ?? []);
  const yvals = asArr(full.y ?? []);
  const n = Math.max(xvals.length, yvals.length);

  // customdata should be per-point; but we accept nested arrays too
  const cd = asArr(full.customdata ?? []);
  // filterMulti[j] = array of filter values for that point (usually length 1)
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

// Persist the full original so later code never depends on filtered gd.data
gd._extmenu_original = original;

// Free memory: remove the full-data payload from trace meta after capture
for (let i = 0; i < (gd.data || []).length; i++) {
  const tr = gd.data[i];
  if (tr && tr.meta && tr.meta.extmenu_full) {
    delete tr.meta.extmenu_full;
    if (Object.keys(tr.meta).length === 0) delete tr.meta;
  }
}
const ORIG = gd._extmenu_original || original;

  // ---- Unique filter values across all points (flattened) ----
  const allF = uniq(
    ORIG.flatMap(o => o.filterMulti.flatMap(vs => vs))
  ).sort((a,b)=>a.localeCompare(b));

  // ---- Initial selected logic (per your rules) ----
  function initSelected() {
    // no cap -> select all
    if (!maxItems) return allF.slice();

    // cap given + cfg.selected not null -> use cfg.selected (trim to cap)
    if (cfg.selected != null) {
      const want = asArr(cfg.selected).filter(v => v != null).map(v => '' + v);
      // keep only values that exist in allF, preserve order, unique, cap
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

    // cap given + cfg.selected is null -> first [max-items] traces (trace order)
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

    // fallback (e.g. no values found somehow)
    if (out.length === 0) return allF.slice(0, Math.min(maxItems, allF.length));
    return out;
  }

  const selected = new Set(initSelected());

  // ---- Popup overlay ----
  gd.style.position = gd.style.position || 'relative';

  const wrap = document.createElement('div');
  wrap.id = cfg.id;
  wrap.style.cssText = `
    position:absolute;
    top:6%;
    left:6%;
    width:50%;
    max-height:80%;
    overflow:auto;
    z-index:9999;
    display:none;
    background:${cfg.box?.background || '#fff'};
    border:${cfg.box?.border ? `${cfg.box.border_width}px solid ${cfg.box.border}` : 'none'};
    box-shadow:0 4px 8px ${cfg.box?.background || '#000'};
    font-family:${cfg.box?.font.family || 'inherit'};
    color:${cfg.box?.font.color || 'inherit'};
    padding:10px;
    border-radius:8px;
  `;
  gd.appendChild(wrap);

  (function makeDraggable(box, container) {
    box.style.cursor = 'default';

    const dragBar = document.createElement('div');
    dragBar.className = 'drag-bar';
    dragBar.style.cssText = `
      width:100%;
      cursor:move;
      user-select:none;
      margin-bottom:6px;
      font-weight:bold;
    `;
    dragBar.innerHTML = cfg.grip || dragBar.textContent;
    box.prepend(dragBar);

    let isDragging = false;
    let startX, startY, startLeft, startTop;

    dragBar.addEventListener('mousedown', (e) => {
      isDragging = true;
      startX = e.clientX;
      startY = e.clientY;

      const rect = box.getBoundingClientRect();
      const parentRect = container.getBoundingClientRect();

      startLeft = rect.left - parentRect.left;
      startTop  = rect.top  - parentRect.top;

      document.body.style.userSelect = 'none';
    });

    document.addEventListener('mousemove', (e) => {
      if (!isDragging) return;

      const dx = e.clientX - startX;
      const dy = e.clientY - startY;

      const parentRect = container.getBoundingClientRect();

      let newLeft = startLeft + dx;
      let newTop  = startTop  + dy;

      newLeft = Math.max(0, Math.min(newLeft, parentRect.width  - box.offsetWidth));
      newTop  = Math.max(0, Math.min(newTop,  parentRect.height - box.offsetHeight));

      box.style.left = newLeft + 'px';
      box.style.top  = newTop  + 'px';
    });

    document.addEventListener('mouseup', () => {
      isDragging = false;
      document.body.style.userSelect = '';
    });
  })(wrap, gd);

  // Close button
  const closeBtn = document.createElement('span');
  closeBtn.innerHTML = cfg.close || '&times;';
  closeBtn.style.cssText = `
    position:sticky;
    top:0;
    float:right;
    cursor:pointer;
    margin-bottom: 5px;
    line-height:1;
    margin-left:10px;
    border-radius:50%;
  `;
  if (cfg.btn?.background) closeBtn.style.background = cfg.btn.background;
  if (cfg.btn?.font.size) closeBtn.style.fontSize = cfg.btn.font.size + 'px';
  if (cfg.box?.font.color) closeBtn.style.color = cfg.btn.font.color;
  if (cfg.box?.font.family) closeBtn.style.fontFamily = cfg.btn.font.family;
  closeBtn.addEventListener('click', () => { wrap.style.display = 'none'; });
  wrap.querySelector('.drag-bar')?.appendChild(closeBtn);

  // ---- Limit indicator / warning (shown only when max-items exists and != 1) ----
  const limitBox = document.createElement('div');
  limitBox.style.cssText = `
    width:100%;
    margin: 4px 0 10px 0;
    font-size: 0.95em;
    opacity: 0.9;
  `;
  const limitText = document.createElement('div');
  const limitWarn = document.createElement('div');
  limitWarn.style.cssText = `margin-top:4px; display:none;`;
  limitBox.appendChild(limitText);
  limitBox.appendChild(limitWarn);
  if (maxItems && maxItems !== 1) wrap.appendChild(limitBox);

  function showLimitWarn(msg) {
    limitWarn.textContent = msg;
    limitWarn.style.display = 'block';
    clearTimeout(showLimitWarn._t);
    showLimitWarn._t = setTimeout(() => { limitWarn.style.display = 'none'; }, 1600);
  }

  function updateLimitUI() {
    if (!(maxItems && maxItems !== 1)) return;
    limitText.textContent = `${cfg.selected_label || 'Valittu'}: ${selected.size} / ${maxItems}`;
  }

  // ---- Toggle all/none button (hidden when max-items === 1) ----
  let toggleBtn = null;
  if (!(maxItems === 1)) {
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
    if (cfg.btn?.font.size) toggleBtn.style.fontSize = cfg.btn.font.size + 'px';
    if (cfg.btn?.font.color) toggleBtn.style.color = cfg.btn.font.color;
    if (cfg.btn?.font.family) toggleBtn.style.fontFamily = cfg.btn.font.family;
    wrap.appendChild(toggleBtn);
  }

  // ---- Checkbox grid ----
  const grid = document.createElement('div');
  grid.style.cssText = 'display:flex; flex-wrap:wrap; gap:8px 16px; width:100%; align-items:flex-start;';
  wrap.appendChild(grid);
  
  // ---- Selection chip under modebar (dismissable) ----
let chipDismissed = false;

const chip = document.createElement('div');
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
  border-radius: 8px;
  opacity: 0.7;
  background:${cfg.box?.background || '#fff'};
  border:${cfg.box?.border ? `${cfg.box.border_width}px solid ${cfg.box.border}` : 'none'};
  box-shadow:0 4px 8px ${cfg.box?.background || '#000'};
  font-family: ${cfg.box?.font.family || 'inherit'};
  color: ${cfg.box?.font.color || 'inherit'};
  font-size: ${cfg.box?.font.size || 12}px;
  line-height:1.2;
  user-select:none;
  white-space:nowrap;
  overflow:hidden;
`;
gd.appendChild(chip);

const chipTextEl = document.createElement('span');
chipTextEl.style.cssText = `
  overflow:hidden;
  text-overflow:ellipsis;
  white-space:nowrap;
`;
chip.appendChild(chipTextEl);

// Close (dismiss) button on chip
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

// Clicking the chip text opens the popup (optional)
chipTextEl.style.cursor = 'pointer';
chipTextEl.addEventListener('click', () => { wrap.style.display = 'block'; });

// Clicking X dismisses chip until the next selection change
chipClose.addEventListener('click', (e) => {
  e.stopPropagation();
  chipDismissed = true;
  chip.style.display = 'none';
});

// Position chip just below modebar
function positionChip() {
  const mb = gd.querySelector('.modebar');
  const mbH = mb ? mb.getBoundingClientRect().height : 0;
  chip.style.top = (mbH + 8) + 'px';
}
gd.on('plotly_relayout', positionChip);
window.addEventListener('resize', positionChip);

// Format selection summary for chip
function chipSummary() {
  if (selected.size === allF.length) return null; // hide when 'all'

  const vals = Array.from(selected);

  if (maxItems === 1) {
    return `${cfg.col}: ${vals[0] ?? ''}`.trim();
  }

  const shown = vals.slice(0, 3);
  const more = vals.length - shown.length;
  return `${cfg.col}: ${shown.join(', ')}${more > 0 ? ` (+${more})` : ''}`;
}

// Update chip: called from applyFilter()
// - If selection changed, chip returns (chipDismissed reset)
// - If chipDismissed and no change, keep hidden
let _lastChipKey = null;

function getCategoryOrderFromCfgOrOrig(axName) {
  // 1) Explicit order from cfg (e.g. factor levels from R)
  if (cfg.categoryorder && Array.isArray(cfg.categoryorder) && cfg.categoryorder.length) {
    return cfg.categoryorder.map(v => '' + v);
  }

  // 2) Fallback: derive from original data order (preserves whatever Plotly got from R)
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

function updateChip(forceShow = false) {
  const txt = chipSummary();

  // create a stable key representing current selection
  const key = Array.from(selected).sort().join('\\u0001');

  // detect a selection change
  const changed = (key !== _lastChipKey);
  _lastChipKey = key;

  // If selection changed, chip should come back
  if (changed) chipDismissed = false;

  if (!txt || (chipDismissed && !forceShow)) {
    chip.style.display = 'none';
    return;
  }

  chipTextEl.textContent = txt;
  chip.style.display = 'flex';
  positionChip();
}


  function updateToggleLabel() {
    if (!toggleBtn) return;

    if (!maxItems) {
      toggleBtn.textContent =
        (selected.size === allF.length) ? (cfg.deselect || 'Poista valinnat') : (cfg.select || 'Valitse kaikki');
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
      for (let i=0; i<update.x.length; i++) {
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
    Plotly.relayout(gd, {'autosize': true});
    updateLimitUI();
    updateToggleLabel();
    updateChip();
  }

  function renderGrid(grid) {
    grid.innerHTML = '';

    allF.forEach(v => {
      const label = document.createElement('label');
      label.style.cssText = `
        display:flex;
        align-items:center;
        gap:6px;
        cursor:pointer;
        width:clamp(120px, 22vw, 240px);
      `;

      const cb = document.createElement('input');
      cb.type = 'checkbox';
      cb.checked = selected.has(v);
      cb.style.accentColor = cfg.checkmark?.color || '#666';
      cb.style.transform = 'scale(' + (cfg.checkmark?.size || 1) + ')';

      cb.addEventListener('change', () => {
        // max-items === 1: radio-like
        if (maxItems === 1) {
          if (cb.checked) {
            selected.clear();
            selected.add(v);
          } else {
            selected.delete(v);
          }
          renderGrid(grid);
          applyFilter();
          return;
        }

        // max-items > 1: enforce cap
        if (cb.checked) {
          if (maxItems && selected.size >= maxItems) {
            cb.checked = false; // revert
            showLimitWarn(cfg.limit_reached || `Enintään ${maxItems} valintaa`);
            return;
          }
          selected.add(v);
        } else {
          selected.delete(v);
        }

        applyFilter();
      });

      const span = document.createElement('span');
      span.textContent = v;

      label.appendChild(cb);
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
        margin-bottom:${cfg.btn?.font.size ? Math.round(cfg.btn.font.size/2) + 'px' : '5px'};
        font-size:${cfg.btn?.font.size ? (cfg.btn.font.size + 2) + 'px' : 'inherit'};
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
        // with a cap: button becomes \"clear / select first N\"
        if (selected.size > 0) {
          selected.clear();
        } else {
          // \"first N traces\" logic (same as initSelected when cfg.selected is null)
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

  let rearray = keys.reduce(function (obj2, key) {
    if (key in thearray) // line can be removed to make it inclusive
    obj2[key] = thearray[key];
    return obj2;

  }, {});

  return rearray;

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
    relayout_array = getVerticalLayout(gd, legend_fontsize, false, keys = ['legend.font.size','legend.orientation','legend.x','legend.y','margin.t','margin.b','margin.r','yaxis.tickfont.size','yaxis2.tickfont.size','images[0].sizey'], pie_chart = pie_chart, logo = logo, tidy_legend = tidy_legend, legend_position = legend_position)
    relayout_array = findCaptionSpace(gd, logo, pie_chart, relayout_array, titlespace);
    Plotly.relayout(gd, relayout_array);
    relayout_array = getVerticalLayout(gd, legend_fontsize, false, keys = ['legend.font.size','legend.orientation','legend.x','legend.y','margin.t','margin.b','margin.r','yaxis.tickfont.size','yaxis2.tickfont.size','images[0].sizey'], pie_chart = pie_chart, logo = logo, tidy_legend = tidy_legend, legend_position = legend_position)
    relayout_array = findCaptionSpace(gd, logo, pie_chart, relayout_array, titlespace);
    Plotly.relayout(gd, relayout_array);
    relayout_array = getVerticalLayout(gd, legend_fontsize, false, keys = ['legend.font.size', 'margin.t', 'margin.b','legend.orientation','legend.x','legend.y','images[0].sizey','yaxis.tickfont.size','yaxis2.tickfont.size'], pie_chart = pie_chart, logo = logo, tidy_legend = tidy_legend, legend_position = legend_position);
    relayout_array = findCaptionSpace(gd, logo, pie_chart, relayout_array, titlespace);
    Plotly.relayout(gd, relayout_array);
    relayout_array = getVerticalLayout(gd, legend_fontsize, false, keys = ['legend.font.size', 'margin.t', 'margin.b','legend.orientation','legend.x','legend.y','images[0].sizey','yaxis.tickfont.size','yaxis2.tickfont.size'], pie_chart = pie_chart, logo = logo, tidy_legend = tidy_legend, legend_position = legend_position);
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


function yrangeRelayout(eventdata, gd, timerId, trace_sums) {

  if (Object.prototype.toString.call(eventdata['xaxis.range']) === '[object Array]' | 'xaxis.range[0]' in eventdata | "xaxis.autorange" in eventdata) {
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
