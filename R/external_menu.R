#' Set up an external menu for filtering categorical y-axis values
#' @importFrom htmlwidgets onRender
#' @importFrom stringr str_glue
#' @importFrom lifecycle signal_stage
#' @export
#' @param p A plotly object created with [roboplot()].
#' @param btn A list defining the appearance and labels of the toggle button.
#' @param items A list defining the appearance and behavior of the checkbox items.
#' @return A plotly object with an external menu for filtering categorical y-axis values.
#' @examples
#'  # Basic usage with default settings
#' energiantuonti |>
#'   dplyr::filter(time == max(time)) |>
#'   roboplot(
#'     Suunta,
#'     plot_type = "bar",
#'     plot_mode = "horizontal",
#'     plot_axes = set_axes(x = "value", y = "Alue")
#'   ) |>
#'   set_external_menu()

set_external_menu <- 
  function(p,
           btn = list(
             box = set_infobox(),
             font = set_font(color = getOption("roboplot.infobox")$color),
             labs = list(all = "Valitse kaikki", none = "Poista valinnat")
           ),
           items = list(
             font = set_font(),
             checkbox = list(
               color = getOption("roboplot.colors.traces")[1],
               size = 1
             ),
             width = list(min = 80, max = 180)
           )
) {

  if(p$plot_mode != "horizontal") {
    roboplotr_alert("set_external_menu() only works with horizontal plots at this time. Returning the original plot.")
    return(p)
  } else {
    roboplotr_message(
      str_c(
        "set_external_menu() is experimental and might be altered or removed completely.",
        "Use at your own discretion.",
        "Will not work with roboplot(artefacts) in the resulting file.",
        "Use roboplot(data) |> set_external_menu() |> create_widget() at this time.",
        collapse = " "
      )
    )
  }

  p |> 
      onRender(jsCode = 
  str_glue("function(el, x) {
  const gd = el;

  const uniq = arr => Array.from(new Set(arr));
  const asArr = v => Array.isArray(v) ? v : [v];

  // ---- Capture original data ----
  const original = (gd.data || []).map(tr => {
    const y = asArr(tr.y ?? []);
    const xvals = asArr(tr.x ?? []);

    const keepParallel = (obj, path) => {
      try {
        const parts = path.split('.');
        let cur = obj;
        for (const p of parts) cur = cur?.[p];
        if (cur == null) return null;
        const a = asArr(cur);
        return (a.length === y.length) ? a.slice() : null;
      } catch(e) { return null; }
    };

    return {
      x: xvals.slice(),
      y: y.slice(),
      text: keepParallel(tr, 'text'),
      hovertext: keepParallel(tr, 'hovertext'),
      customdata: keepParallel(tr, 'customdata')
    };
  });

  const allY = uniq(original.flatMap(o => o.y)).sort((a,b)=>(''+a).localeCompare(''+b));
  const selected = new Set(allY);

  // ---- Create container UNDER plot ----
  const wrap = document.createElement('div');
  wrap.style.cssText = 'margin-top:12px;';
  gd.appendChild(wrap);
  
  function syncSideMarginsAndYStyle() {
  const fl = gd._fullLayout;
  if (!fl || !fl.margin) return;

  // 1) Match plot side margins
  wrap.style.paddingLeft  = fl.margin.l + 'px';
  wrap.style.paddingRight = fl.margin.r + 'px';

  // 2) Match y-axis tick font (resolved)
  const ya = fl.yaxis || {};
  const tf = ya.tickfont || {};
  // tickfont.color can be undefined; fall back to global font color if present
  const fallbackColor = (fl.font && fl.font.color) ? fl.font.color : null;
  // Apply to the whole grid so labels inherit it
  wrap.style.fontFamily = '<{items$font$family}';
  if (tf.size)   wrap.style.fontSize   = tf.size + 'px';
  wrap.style.color = '<{items$font$color}';

  // Optional: mimic axis tick text spacing vibe
  // (Plotly uses no special letter spacing, so usually unnecessary)
}


  // ---- Toggle Button ----
  const toggleBtn = document.createElement('button');
  toggleBtn.style.cssText = `
    width:100%;
    padding:8px 10px;
    margin-bottom:10px;
    cursor:pointer;
    border-radius:6px;
    border:1px solid #ccc;
    background:<{btn$box$background};
  `;
    toggleBtn.style.fontSize   = '<{btn$font$size}px';
    toggleBtn.style.color      = '<{btn$font$color}';
    toggleBtn.style.marginRight      = 'inherit';
  wrap.appendChild(toggleBtn);

  // ---- Checkbox Flex Grid ----
  const grid = document.createElement('div');
  grid.style.cssText = `
    display:flex;
    flex-wrap:wrap;
    gap:8px 16px;
    width:100%;
    align-items:flex-start;
  `;
  wrap.appendChild(grid);

  function updateToggleLabel() {
    toggleBtn.textContent =
      (selected.size === allY.length) ? '<{btn$labs$none}' : '<{btn$labs$all}';
  }

  function applyFilter() {
    const idxs = gd.data.map((_, i) => i);
    const update = { x: [], y: [] };

    const hasText = original.some(o => o.text);
    const hasHover = original.some(o => o.hovertext);
    const hasCustom = original.some(o => o.customdata);

    if (hasText) update.text = [];
    if (hasHover) update.hovertext = [];
    if (hasCustom) update.customdata = [];

    for (let ti = 0; ti < original.length; ti++) {
      const o = original[ti];
      const keepIdx = [];
      for (let j = 0; j < o.y.length; j++) {
        if (selected.has(o.y[j])) keepIdx.push(j);
      }

      update.x.push(keepIdx.map(j => o.x[j]));
      update.y.push(keepIdx.map(j => o.y[j]));

      if (hasText) update.text.push(o.text ? keepIdx.map(j => o.text[j]) : null);
      if (hasHover) update.hovertext.push(o.hovertext ? keepIdx.map(j => o.hovertext[j]) : null);
      if (hasCustom) update.customdata.push(o.customdata ? keepIdx.map(j => o.customdata[j]) : null);
    }
    const visibleCats = allY.filter(v => selected.has(v));
  
    Plotly.relayout(gd, {
      'yaxis.categoryorder': 'array',
      'yaxis.categoryarray': visibleCats
    });
    Plotly.restyle(gd, update, idxs);
    updateToggleLabel();
  }

  function renderGrid() {
    grid.innerHTML = '';

    allY.forEach(v => {
      const label = document.createElement('label');
      label.style.cssText = `
        display:flex;
        align-items:center;
        gap:6px;
        cursor:pointer;
        width:clamp(<{items$width$min}px, 22vw, <{items$width$max}px);
      `;

      const cb = document.createElement('input');
      cb.type = 'checkbox';
      cb.checked = selected.has(v);
      cb.style.accentColor = '<{items$checkbox$color}';
      cb.style.transform = 'scale({items$checkbox$size})';
      cb.addEventListener('change', () => {
        if (cb.checked) selected.add(v);
        else selected.delete(v);
        applyFilter();
      });

      const span = document.createElement('span');
      span.textContent = '' + v;

      label.appendChild(cb);
      label.appendChild(span);
      grid.appendChild(label);
    });

    updateToggleLabel();
  }

  toggleBtn.addEventListener('click', () => {
    if (selected.size === allY.length) {
      selected.clear();
    } else {
      allY.forEach(v => selected.add(v));
    }
    renderGrid();
    applyFilter();
  });
  
setTimeout(syncSideMarginsAndYStyle, 0);
gd.on('plotly_relayout', syncSideMarginsAndYStyle);
window.addEventListener('resize', syncSideMarginsAndYStyle);

  renderGrid();
  applyFilter();
}
",.open = "<{") )
  
  }