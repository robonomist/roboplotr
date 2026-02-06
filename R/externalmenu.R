#' Set up an external menu for filtering plot data based on a categorical variable.
#' This function creates a configuration for an external menu that can be used to filter plot data based on a categorical variable. The menu will be rendered as a draggable overlay on the plot, with checkboxes for each unique value of the specified variable. The menu can be toggled from the modebar.
#' @param col Symbol or character. Column from `d` of `roboplot()` that contains the categorical variable for filtering. This column will be used to create the checkboxes in the menu.
#' @param title Logical. Whether to include a title in the menu. Default is TRUE.
#' @param box A list of styling options for the menu box. Use `set_infobox()` to create this list.
#' @param btn A list of styling options for the box selector. Use ` set_infobox()` to create this list.
#' @examples
#' 
#' # You might want to make items selectable outside of the legend selection. Use
#' # the parameter `externalmenu` with `set_externalmenu()` to provide the column 
#' # that controls visiblity of items in the plot available from the `roboplot` 
#' # modebar. See more examples in the `set_externalmenu()` documentation.
#' 
#' energiantuonti |>
#'   dplyr::filter(time == max(time)) |>
#'   roboplot(Suunta, plot_type = "bar", plot_mode = "horizontal",
#'            plot_axes = set_axes(x = "value", y = "Alue"),
#'            externalmenu = set_externalmenu(Alue)
#'   )
#' 
#' 
#' # You might want to make items selectable outside of the legend selection. Use
#' # the parameter `externalmenu` with `set_externalmenu()` to provide the column 
#' # that controls visiblity of items in the plot. This way you can construct, 
#' # for example, a horizontal bar plot where you have the legend controlling
#' # the color variable, but still filter the items in the y-axis through the 
#' # external menu, available from the `roboplot` modebar.
#' 
#' energiantuonti |>
#'   dplyr::filter(time == max(time)) |>
#'   roboplot(Suunta, plot_type = "bar", plot_mode = "horizontal",
#'            plot_axes = set_axes(x = "value", y = "Alue"),
#'            externalmenu = set_externalmenu(Alue)
#'            )
#' 
#' # You can combine external menu with pattern to have the pattern variable 
#' # controlled by the external menu.
#' 
#' energiantuonti |>
#'   roboplot(Alue,
#'            pattern = Suunta,
#'            externalmenu = set_externalmenu(Suunta)
#'   )
#' 
#' # You could also use the external menu to control the visibility of items in the 
#' # legend to save space on the plot itself. This is not as obvious to user and 
#' # the external menu won't have the legend colors available, but can be useful 
#' # in some situations.
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, 
#'            legend = set_legend(position = "none"),
#'            externalmenu = set_externalmenu(Alue)
#'   )
#' 
#' # Any arbitary column works, but the results in the plot are up to the user.
#' 
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Vienti") |>
#'   dplyr::mutate(menu = purrr::map_chr(Alue, ~ sample(c("a","b","c","d"),1))) |>
#'   roboplot(Alue,
#'            externalmenu = set_externalmenu(menu)
#'   )
#' 
#' # You can even filter for the dates, but remember that the external menu is 
#' # not meant for filtering the data, but rather for controlling the visibility 
#' # of items in the plot. If you want to filter the data, it's better to do it 
#' # before plotting, or use something more appropriate like a rangeslider.
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Vienti") |>
#'   roboplot(Alue,
#'            externalmenu = set_externalmenu(time),
#'            rangeslider = T
#'   )
#' 
#' # Or at least construct a reasonable filter for an appropriate plot.
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Vienti", Alue %in% c("Venäjä", "Kanada")) |>
#'   dplyr::mutate(Vuosi = lubridate::year(time)) |>
#'   roboplot(Alue,
#'            plot_type = "bar",
#'            externalmenu = set_externalmenu(Vuosi)
#'   )
#' 
#' # Or some other arbitary grouping
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Vienti") |>
#'   dplyr::mutate(Suunta = ifelse(Alue %in% c("Venäjä","Viro","Latvia"), "Itä", "Länsi")) |>
#'   roboplot(Alue, 
#'            externalmenu = set_externalmenu(Suunta))
#' 
#' # You can control the appearance of the externalmenu somewhat by using 
#' # `set_infobox()` with set_externalmenu(box, btn), or omit the title with 
#' # `set_externalmenu(title = FALSE)`.
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(
#'     Alue,
#'     legend = set_legend(position = "none"),
#'     externalmenu = set_externalmenu(
#'       Alue,
#'       title = F,
#'       box = set_infobox(background = "orange"),
#'       btn = set_infobox(
#'         border = "brown",
#'         border_width = 4,
#'         background = "red"
#'       )
#'     )
#'   )
#' 
#' # Technically, nothing prevents you from using the external menu along with 
#' # legend, but it might be quite confusing for the user.
#' 
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Vienti") |>
#'   roboplot(Alue,
#'            externalmenu = set_externalmenu(Alue)
#'   )
#' 
#' @export
set_externalmenu <- function(
    col = NULL,
    title = TRUE,
    box = set_infobox(background = getOption("roboplot.colors.background")),
    btn = set_infobox()
) {
  
  roboplotr_typecheck(title, "logical", allow_null = F)
  col <- enquo(col)
  if (quo_is_null(col)) return(NULL)
  
  item_font <- set_font(color = box$font)
  btn_font <- set_font(font = getOption("roboplot.font.title")$.font, color = box$font)
  
  box$font <- item_font
  btn$font <- btn_font
  
  title_select <- getOption("roboplot.locale")$externalmenu_labels$select
  title_deselect <- getOption("roboplot.locale")$externalmenu_labels$select
  checkmark_size = 1
  checkmark_color = getOption("roboplot.colors.traces")[1]
  
  .res <- list(
    id = paste0("roboplot-externalmenu-", gsub("\\.", "", as.character(runif(1)))),
    col = col,
    title = title,
    select = title_select,
    deselect = title_deselect,
    box = box,
    btn = btn,
    checkmark = list(size = checkmark_size, color = checkmark_color)
  )
  
  .res <- structure(.res, class = c("roboplotr", "roboplotr.set_externalmenu", class(.res)))
  
  .res
  
}

roboplotr_set_external_menu <- function(p, externalmenu, d_names) {
  
  if (is.null(externalmenu)) return(p)

  roboplotr_typecheck(externalmenu, "set_externalmenu")
  col <- externalmenu$col
  roboplotr_check_valid_var(col, d_names, where = "`set_externalmenu()`")
  
  externalmenu$col <- as_label(externalmenu$col)
  
  externalmenu$close <- fa(
    "times-circle",
    fill = externalmenu$box$font$color,
    height = str_c(externalmenu$btn$font$size+2,"px")
  )
  externalmenu$grip <- fa(
    "grip",
    fill = externalmenu$box$font$color,
    height = str_c(round(externalmenu$btn$font$size*1.5),"px")
  )
  p$x$roboplot_externalmenu <- externalmenu
  
  p |> onRender(
    "function(el, x) {
  const gd = el;
  const cfg = x.roboplot_externalmenu;
  if (!cfg || !cfg.col) return;

  const uniq = arr => Array.from(new Set(arr));
  const asArr = v => Array.isArray(v) ? v : (v == null ? [] : [v]);

  // ---- Normalize a single per-point filter value to scalar OR multiple scalars ----
  // If customdata[j] is an array, we treat it as multiple categories for that point
  // (flatten for unique list), but during filtering we’ll check membership.
  function normVal(v) {
    if (v == null) return [];
    if (Array.isArray(v)) return v.filter(x => x != null).map(x => '' + x);
    return ['' + v];
  }

  // ---- Capture original per-trace arrays ----
  const original = (gd.data || []).map(tr => {
    const xvals = asArr(tr.x ?? []);
    const yvals = asArr(tr.y ?? []);
    const n = Math.max(xvals.length, yvals.length);

    // customdata should be per-point; but we accept nested arrays too
    const cd = asArr(tr.customdata ?? []);
    // filterMulti[j] = array of filter values for that point (usually length 1)
    const filterMulti = [];
    for (let j = 0; j < n; j++) {
      filterMulti.push(normVal(cd[j]));
    }

    // keep parallel arrays
    const text = asArr(tr.text ?? null);
    const hovertext = asArr(tr.hovertext ?? null);

    return {
      x: xvals.slice(),
      y: yvals.slice(),
      filterMulti,
      text: (text.length === n) ? text.slice() : null,
      hovertext: (hovertext.length === n) ? hovertext.slice() : null
    };
  });
  

  // ---- Unique filter values across all points (flattened) ----
  const allF = uniq(
    original.flatMap(o => o.filterMulti.flatMap(vs => vs))
  ).sort((a,b)=>a.localeCompare(b));

  const selected = new Set(allF);

  // ---- Popup overlay (like your infobox modal) ----
  // Ensure gd is a positioning context
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
    padding:10px;
    border-radius:8px;
  `;
  gd.appendChild(wrap);
  
  (function makeDraggable(box, container) {
  box.style.cursor = 'default';

  // create a header bar for dragging
  const dragBar = document.createElement('div');
  dragBar.className = 'drag-bar';
  dragBar.style.cssText = `
    width:100%;
    cursor:move;
    user-select:none;
    margin-bottom:6px;
    font-weight:bold;
  `;
  //dragBar.textContent = '☰'; // or empty if you don’t want text
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

    // Constrain inside parent
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

  // ---- Toggle all/none button ----
  const toggleBtn = document.createElement('button');
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

  // ---- Checkbox grid ----
  const grid = document.createElement('div');
  grid.style.cssText = 'display:flex; flex-wrap:wrap; gap:8px 16px; width:100%; align-items:flex-start;';
  wrap.appendChild(grid);

  function updateToggleLabel() {
    toggleBtn.textContent =
      (selected.size === allF.length) ? (cfg.deselect || 'Poista valinnat') : (cfg.select || 'Valitse kaikki');
  }

  function inferCategoryAxis() {
    const bar = (gd.data || []).find(tr => tr.type === 'bar');
    if (!bar) return null;
    return (bar.orientation === 'h') ? 'yaxis' : 'xaxis';
  }

  function applyFilter() {
    const idxs = gd.data.map((_, i) => i);
    const update = { x: [], y: [] };

    const hasText = original.some(o => o.text);
    const hasHover = original.some(o => o.hovertext);
    if (hasText) update.text = [];
    if (hasHover) update.hovertext = [];

    for (let ti = 0; ti < original.length; ti++) {
      const o = original[ti];
      const keepIdx = [];

      for (let j = 0; j < o.filterMulti.length; j++) {
        // keep if ANY of the point's filter values is selected
        const vs = o.filterMulti[j];
        if (vs.some(v => selected.has(v))) keepIdx.push(j);
      }

      update.x.push(keepIdx.map(j => o.x[j]));
      update.y.push(keepIdx.map(j => o.y[j]));
      if (hasText) update.text.push(o.text ? keepIdx.map(j => o.text[j]) : null);
      if (hasHover) update.hovertext.push(o.hovertext ? keepIdx.map(j => o.hovertext[j]) : null);
    }

    // tighten category axis for bars
    const ax = inferCategoryAxis();
    if (ax) {
      const cats = new Set();
      for (let i=0; i<update.x.length; i++) {
        const tr = gd.data[i] || {};
        if (tr.type !== 'bar') continue;
        const axisCats = (tr.orientation === 'h') ? update.y[i] : update.x[i];
        (axisCats || []).forEach(v => cats.add(v));
      }
      const visibleCats = Array.from(cats).sort((a,b)=>(''+a).localeCompare(''+b));
      const rel = {};
      rel[ax + '.categoryorder'] = 'array';
      rel[ax + '.categoryarray'] = visibleCats;
      Plotly.relayout(gd, rel);
    }

    Plotly.restyle(gd, update, idxs);
    Plotly.relayout(gd, {'autosize': true});
    updateToggleLabel();
  }

  function renderGrid() {
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
        if (cb.checked) selected.add(v);
        else selected.delete(v);
        applyFilter();
      });

      const span = document.createElement('span');
      span.textContent = v;

      label.appendChild(cb);
      label.appendChild(span);
      grid.appendChild(label);
    });
    
    if(cfg.title === true) {
    const titleEl = document.createElement('div');
    titleEl.textContent = cfg.col
    titleEl.style.cssText = `
      width:100%;
      text-align:left;
      font-weight:bold;
      margin-bottom:${cfg.btn?.font.size ? Math.round(cfg.btn.font.size/2) + 'px' : '5px'};
      font-size:${cfg.btn?.font.size ? (cfg.btn.font.size + 2) + 'px' : 'inherit'};
    `;
    grid.prepend(titleEl);
    }

    updateToggleLabel();
  }

  toggleBtn.addEventListener('click', () => {
    if (selected.size === allF.length) selected.clear();
    else allF.forEach(v => selected.add(v));
    renderGrid();
    applyFilter();
  });

  renderGrid();
  applyFilter();
}
"
  )
}


roboplotr_modebar_externalmenu_button <- function(btn_list, externalmenu) {
  if (is.null(externalmenu) || is.null(externalmenu$id)) return(btn_list)
  
  btn <- list("externalmenu" = list(
    name = getOption("roboplot.locale")$modebar_label$filter,
    icon = roboplotr_modebar_dl_icon("filter"),
    click = JS(str_glue(
      "function(gd) {
         var el = document.getElementById('<{externalmenu$id}');
         if (!el) return;
         el.style.display = (el.style.display === 'none' || el.style.display === '') ? 'block' : 'none';
       }", .open = "<{"
    ))
  ))
  
  append_location <- ifelse("robonomist" %in% names(btn_list), length(btn_list) - 1, length(btn_list))
  append(btn_list, btn, after = append_location)
}
