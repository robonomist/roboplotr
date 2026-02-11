# roboplotr 2.8.2.4

* Various validation improvements.

# roboplotr 2.8.2.3

* Extends needed pattern types beyond the plotly defaults ("solid", "dash", "dot" etc.) when the number of category items is large.

# roboplotr 2.8.2.2

* Switches roboplotr relayouting scripts to un-minified version until minifying issues are resolved. This should not cause any issues, but might make the package slightly larger.

# roboplotr 2.8.2.1

* Fixes a `robolot(externalmenu)` preselection issue, and prevents the concurrent usage of `robolot(externalmenu, updatemenu)`.

# roboplotr 2.8.2

* Added params `set_external_menu(selected, max_length)` to control the initial selection and maximum length of items shown in the external menu created with `set_external_menu()`.
* Fixes a `robolot(updatemenu)` issue on render.

# roboplotr 2.8.1.1

* `set_external_menu()` documentation added

# roboplotr 2.8.1

* `set_external_menu()` formalized to be used in `roboplot(externalmenu)` with validation, allowing for an external menu to be used for controlling the visibility of elements in any `roboplot(). See the documentation.
* Adds param `visible` to `set_legend()` to control the initially visible legend items of `roboplot()`s.

# roboplotr 2.8.0

* Adds experimental `set_external_menu()` to supplement `set_updatemenu()`. Currently only usable on horizontal bar plots, and will be eventually integrated into `roboplot()`, or removed completely.

# roboplotr 2.7.12

* Fixes `robotable()` column sorting issue.

# roboplotr 2.7.11

* Adds `reverse` param to `set_legend()` with `roboplot()` to allow for reversing the order of legend items. Does not currently work with `robomap()`.
* Fixes `roboplot()` x-axis not respecting the factor level ordering.

# roboplotr 2.7.10

* Fixes a string transformation bug for roboplot data download modebar button.

# roboplotr 2.7.9

* Fixes a leaflet dependency issue and minor documentation issue.

# roboplotr 2.7.8

* Adds param `robomap(viscosity)` that allows to let `robomap`s snap back to initial view, or prevent dragging altogether.
* Expands param `robomap(zoom)` for finer control of leaflet zoom methods.

# roboplotr 2.7.7

* Changes `set_confidence_interval(show_legend = TRUE)` to override `set_legend(position = "none")` with `position = "bottom"`.
* Rewrite of `roboplotr_transform_data_for_download()` and related internals for much faster handling of special characters in columns to be imported to modebar csv downloads.

# roboplotr 2.7.6

* Adds `set_roboplot_options(table_options)` and the corresponding `set_table_options()` to control `robotable()` appearance options globally. Currently only supports stripe color and opacity, but more will be added later.
* Fixes `robotable()` not respecting fonts set from files.

# roboplotr 2.7.5

* Changes `alt`to `aria-label`, and includes the `aria-label` even with `set_title(include = F)` as long as there is a title.

# roboplotr 2.7.4

* Removes `alt` from `set_artefacts()`, moving it to `roboplot()` internal scripts instead.

# roboplotr 2.7.3

* Adds `alt` to `set_artefacts()`, a logical param for whether html-artefacts will get the artefact title as the `alt` attribute.

# roboplotr 2.7.2

* Moves all `leaflet`dependecies to Suggests, with `robomap()` warning if the user does not have `leaflet`.
* Fixes `robotable` not respecting localization with labels. 
* Adds param `labels` to `robotable()` to control the nav labels of the table.

# roboplotr 2.7.1

* Allows for woff files to be used with `set_font()`.
* Adds `roboplot(hole)` to specify hole size for pie plots.
* Adds `set_legend(xref)` to control bottom-positioned legend anchoring in `roboplot()`s, aligning with similar option for `set_title()` and `set_caption()`.
* Soft-deprecates `set_roboplot_options(tidy_legend)` in favor of using `set_roboplot_options(caption, title)`.

# roboplotr 2.7.0

* Hard deprecates `roboplot(error_bars, legend_position, legend_maxwidth)` in favor of using and `set_confidence_interval()` and `set_legend()`.
* Adds `set_roboplot_options(axes = set_axes())` for some plot axis parameters.
* Minor grid line color can now be set with `set_roboplot_options(grid = set_grid())`.
* Soft-deprecates `set_roboplot_options(font_caption, font_title)` in favor of using `set_roboplot_options(caption, title)`.
* HTML widget background colors now use `set_roboplot_options(background)`. 

# roboplotr 2.6.8

* `roboplot()`s have now detection for proper padding for numeric axis limits set with `roboplot(plot_axes = set_axes()`. 

# roboplotr 2.6.7

* `roboplot()`s now only trigger relayout when the plot itself is visible on viewport. 

# roboplotr 2.6.6

* Fixes an issue where `roboplot()` fails to detect logo size correctly. 
* Fixes an issue where `roboplot(plot_type = "pie")` fails to position caption and logo correctly.

# roboplotr 2.6.5

* Fixes an issue where `set_roboplot_options()` fails to detect fonts stored as files inside a package on load.

# roboplotr 2.6.4

* Improved handling of HTML from plot elements in `roboplot()` csv files.

# roboplotr 2.6.3

* Fixes `robotable()` sorting `Date` columns as characters instead of dates. 
* Factor columns are now sorted by levels in `robotable()`s.
* Improves `caption` space handling in `roboplot()`s.
* `roboplot()`s now include all of `title`, `caption` and `info_text` in downloaded csv files.
* Improved handling of special characters in `roboplot()` csv files.

# roboplotr 2.6.2

* Fixes `set_roboplot_options()` crashing if modebar download buttons are not uniquely named, instead reverting to defaults.

# roboplotr 2.6.1

* Fixes a `robotable()` error when using `responsive = NULL`, and `robotable()` bottom margins.

# roboplotr 2.6.0

* Adds `empty_roboplot` to `set_roboplot_options()` where the appearance of empty roboplots can be controlled with `set_empty_roboplot()`.
* Added `responsive` to `robotable()` to allow for collapsing colums where the table is too wide.

# roboplotr 2.5.2

* Fixes `roboplot()`s sometimes losing legends on render with no apparent reason, only to reappear.
* Fixes `roboplot()` failing to handle frequency attribute of length 2+.
* Fixes `roboplot(confidence_area = set_confidence_area("area"), plot_mode = "smooth")` failing to draw the confidence area.
* `roboplot()` legend size is now tied to the size of legend font size.

# roboplotr 2.5.1

* Fixes a `roboplot()` legend display issue.

# roboplotr 2.5.0

* Adds `label` param to `roboplot()` along with the corresponding `set_label()` function for trace labeling control.
* Fixes `roboplot(plot_mode = "horizontalfill")` not sorting the bars correclty by `color`.

# roboplotr 2.4.7

* Adds `zoom` parameter to `set_modebar()` to control the level of zoom of images downloaded from `roboplot()` modebars.

# roboplotr 2.4.6

* Fixes `set_legend()` issue with `robomap()`.
* Fixes factor level issue with `robomap()` when map geometry values are factors.

# roboplotr 2.4.5

* Fixes autorange relayouts for `roboplot()`s and various other axis range issues.
* Fixes `robomap()` legends not having the proper range when all values in map data fall between <1 and >-1.

# roboplotr 2.4.4

* Fixes `set_caption()` not working as expected when used with `set_roboplot_options()`.

# roboplotr 2.4.3

* Fixes `roboplot()` pies broken by 2.4.2.

# roboplotr 2.4.2

* Allows for artefacts downloaded through the resulting html `roboplot`to be named separately from the plot with `set_modebar(title)`, and set the artefact titles globally with `set_roboplot_options()`.
* Fixes `roboplot()` not respecting globally set `set_title(xref)`.
* Improvements in `roboplot(xaxis_ceiling)`, the shorthand property that works similarly to `set_axes(xlim)`. 
* Improved behavior of `roboplot()` `zeroline`s and `shadearea`s.

# roboplotr 2.4.1

* Fixes `roboplot()` too tight top margin when there is a subtitle without a title.
* Makes it explicit that `roboplot()` must be provided either `color`, or `title` with string length of 1+.

# roboplotr 2.4.0

* Fixes `robotable()` date columns and `roboplot()` data downloads for dates not formatting leading zeroes properly on windows.
* Adds the options to anchor `roboplot()` titles and captions to left container edge instead of left plot edge with param `xref` in `set_title()` and `set_caption()` respectively. You can set them globally with `set_roboplot_options()`.
* Makes `set_roboplot_option(override_webshot)` default to FALSE.
* Removes the necessity of manually setting ticktypes with `set_axes()` when you are not using a numeric "value" and date "time"-columns.

# roboplotr 2.3.2

* Fixes `robotable()` not being able to handle data with no numeric columns.
* Fixes `robomap()` having duplicate hover labels when values and areas refer to same factor levels.
* Fixes `robomap()` not respecting `caption = NA` properly.

# roboplotr 2.3.1

* `create_widget()` now creates the directory if it does not exist. 
* `create_widget()` is now more verbose.
* `create_widget()` and `set_artefacts()` now have a `delay` parameter for static file creation, allowing `webshot2::webshot()` more time when necessary.

# roboplotr 2.3.0

* Added option to save named roboplot options with `set_roboplot_options(.defaults = "name")` and load them with `set_roboplot_options(reset = "name")`.
* Added option to use these named roboplot options directly in `roboplot()`s with `roboplot(roboplot_options = "name")`.
* Fixes `create_widget()` not respecting sizing.
* `create_widget()` now overrides `new_session_screenshot()` from `webshot2` to correctly scale pdf artefacts.
* Some improved axis controls in `set_axes()`.

# roboplotr 2.2.1

* Plotly version updated to 2.35.2 to allow for `layer = "between"` for zerolines and shadeareas. Not yet supported when used in shinyapps.
* Fixes `roboplot()` not respecting `zeroline` when it is set to `F`.
* Fixes `roboplot()` not omitting legend when `legend` is not specified and trace number is 1.
* Adds zeroline color and width control to `roboplot()` in addition to setting them globally with `set_roboplot_options()`.
* Fixes `create_widget()` not using hidden plot titles as default filenames.
* Adds option to omit the caption from `roboplot` with `caption = NA` (or `caption = set_caption(NA)`).
* Adds option to specify tick label angle with `plot_axes(xangle, yangle)`.
* Adds option to somewhat control x-axis labels with `plot_axes(xanchor, xstep)` when the x-axis consists of dates.

# roboplotr 2.1.4

* Removed `robomap()` modebar button for static map downloads, as shinyapps and chrome have issues with it.
* Fix `robomap()` logo positioning.

# roboplotr 2.1.3

* Fix for roboplot artefact dimensions.
* Improves `robomap()` legend and color controls, and allows for factor data in the value column of the used data.

# roboplotr 2.1.2

* Fixes for `robomap()` labeling and map downloads.

# roboplotr 2.1.1

* `roboplot()` `confidence_interval`s now accepts column labeling the y-axis values as a factor level for the intervals, allowing for just pivoting your data. You can still use NA for the label referring to baseline.
* Various visual changes in `robomap()` to better align it with visuals of `roboplot`s and `robotable`s.

# roboplotr 2.1.0

* Deprecation of `error_bars` param in `roboplot()` and the related function `set_errorbars()` in favor of `confidence_interval` and `set_confidence_interval()`. The new function handles both error bars and continuous areas. 
* Added `set_updatemenus()` for `roboplot()` to allow for more dropdowns for controlling displayed traces. Especially useful with a large number of legend items.
* Fixes `set_pattern()` param `pattern_along` not working properly in all use cases.
* Moved several Imports to Suggests, mainly for `robomap()`.

# roboplotr 2.0.2

* Added option of providing column-speficic rounding for `robotable()`. Global defaults for rounding can now be specified with `set_roboplot_options()`.

# roboplotr 2.0.1

* Fix for `set_hovertext()` param `text_col` affecting legend labeling.
* Fix for `robotable()` rounding dropping decimals from whole numbers instead of using the given rounding.

# roboplotr 2.0.0

* `shinyapp`-parameter in `roboplot()` and `set_roboplot_options()` has been reworked to align with how it actually works. This is a breaking change.
* Changes in `roboplotr` internal validation. This is a breaking change.
* Fix for Google fonts not showing on `robotable()```. Loosened font restrictions.
* Added preDraw callback to `robotable()` that forces core DataTables css to load properly.
* Improved `roboplot()` logo handling when set to "none".
* Added `set_legend()` for specifying `robomap()` and `roboplot()` legend appearance.
* Added support for `legend_position` = "right" for `roboplot()`.

# roboplotr 1.8.3

* Static image downloads font size and top margin fix.

# roboplotr 1.8.2

* Fixes `roboplot()` pie plots erroneously trying to use a secondary_yaxis.

# roboplotr 1.8.1

* Fixes documentation entries, data ordering issues and language.

# roboplotr 1.8

* robotable now correctly displays background color.
* roboplot relayout fixes for various plot types.
* roboplot legend order fix to respect factor levels both for legend and trace order (first level is the topmost trace, bar traces are behind scatter and line traces). Using secondary_yaxis will mess up trace order on plots, but not on legends.
* added function set_modebar() for modebar control in roboplots, adding visibility controls both globally and by plot. Not yet supported by robotable.
* set_roboplot_options() can now save current options as defaults with .defaults = T, and reset = T uses these defaults if they are set. You can also use reset = T simultaneously with setting new options.
* added option of using set_roboplot_options() param logo_file as "none".

# roboplotr 1.7.1

* Improved plot rescaling
* Modebar and rangeslider properly removed from static plots.

# roboplotr 1.7

* Added parameter tidy_legend to roboplot(). You might what to make all legend items the same width, regardless of the string length. It is probably better to make the plots tall enough, as wider legend items might cause issues when resizing.
* Reworked internal resizing scripts to handle corner cases better, and improved scaling of y-axis of shadeareas, and the plot when using a rangeslider.

# roboplotr 1.6

* Completely reworked how secondary y-axis is added to roboplot()s, with much more fine-grained control and improved handling of yaxis when resizing.
* Reworked map palette specifications for robomap() to accept set_heatmap() when you want to explicitly set color breaks, and added legend_breaks where you can either specify the number of legend items or the breakpoints of robomap().
* Added more font controls to set_axes().

# roboplotr 1.5.1.1

* set_pattern showlegend hotfix.

# roboplotr 1.5.1

* set_pattern continuous pattern bug fix.
* set_pattern showlegend fix when legend_position is set to NA (no legend).

# roboplotr 1.5

* robotable options for enabling / disabling filtering and searching, and column width control.
* robotable font size fix

# roboplotr 1.4.11

* robotable set_heatmap option added to define color for NA values.
* robotable option added to control table class from given selection.

# roboplotr 1.4.10

* Fix for internal text color picker.
* roboplot option to specify only a single trace color added.
* robomap option to disable zoom added.

# roboplotr 1.4.9

* Roboplot dependency and css fixes for Quarto renders.
* Artefact creation integration to robotable.
* Documentation and example fixes.
* Improved accessibility for label text colors.

# roboplotr 1.4.8

* Legend item horizontal spacing improvement.

# roboplotr 1.4.7

* roboplot hoverlabel fixes for font, color and xhoverlabel.

# roboplotr 1.4.6

* roboplot scripts for legend layout improved.
* robomap option to omit legend added.
* robotable title tag type change for quarto compatibility.

# roboplotr 1.4.5

* robotable parameter dateformat added for user-defined date formats.

# roboplotr 1.4.4

* robotable lengthMenu better aligned with data
* set_pattern to give more verbose message to alert user of the usage.

# roboplotr 1.4.3

* Caption and logo positioning fix when no legend exists.
* Removed the need for specified container when running shiny apps. Roboplot creates an observer to detect changes in visibility.
* Shadearea accounts for xaxis upper bound if one exists.

# roboplotr 1.4.2

* Axes with ticktype of "character" sorting fixed.
* Added the "horizontalstack" for plot_mode options available for a horizontal bar plot.
* Added the "markers" as parameter for roboplot, along with set_marker() to use it.
* Added the "error_bars" as parameter for roboplot, along with set_errorbars() to use it.
* roboplot() xaxis_ceiling fix.
* legend, caption and logo positioning fixes

# roboplotr 1.4.1.1

* minor addition for set_shadearea usage example.

# roboplotr 1.4.1

* create_widget to replace roboplot_create_widget (in name only)
* Fixes for readme examples and pkgdown contents.
* Fix for roboplot scripts to adjust caption and logo position when there are no xaxis labels.
* Numerous documentation fixes.

# roboplotr 1.4.0.1

* Added missing webshot2 dependency.

# roboplotr 1.4

* Completely reworked static file automation, separating it from modebar. Allows for special fonts in static files.
* Added set_shadearea() (and roboplot(shadearea)) to add a highlight area to plots. 
* Reworked zeroline handling somewhat, adding set_zeroline() to use with set_roboplot_options(zeroline) to control the appearance of zeroline.
* Added param text_col to set_hovertext() that can be used to set what roboplot() uses for labeling (mainly the hoverlabels). 
* Info texts improvement (may use HTML). 
* Remove html tags from data when downloading the data from image.
* Improved logo and caption positioning in relation to each other and the legend box.
* Plotly partial bundle switched to 2.28.0 to allow for usage of dashtypes in gridlines (only when set_roboplot_options(shinyapp = F)).

# roboplotr 1.3.4.4

* Improved control for plot mode by trace.

# roboplotr 1.3.4.3

* Font fallback crash fix for websafe fonts.

# roboplotr 1.3.4.2

* Examples fix for set_caption(). Fix for multiple font css definitions. Added Google Font support for fonts. Still won't work with downloaded pngs (or svgs).

# roboplotr 1.3.4.1

* set_locale fix.

# roboplotr 1.3.4

* roboplot_set_options control for infobox appearance. Roboplot fix to accept integer columns for numeric axes.

# roboplotr 1.3.3.3

* Roboplot pattern and dashtype control fix. Roboplot and robotable infobutton popup fixes.

# roboplotr 1.3.3.2

* Roboplot modebar color fix.

# roboplotr 1.3.3.1

* Robomap legend numeric handling fix.

# roboplotr 1.3.3

* Roboplot gets info_text parameter to be shown with the modebar info button. Robomap legend fix, data_contour fix, legend fix, rounding fix. Robotable labeling change, alignment change and infopop to allow HTML tags. Roboplot left margin change, named plot modes bug fix, modebar label right margin fix.

# roboplotr 1.3.2.4

* Hovermode fix.

# roboplotr 1.3.2.3

* Dependency fix.

# roboplotr 1.3.2.3

* Robomap log scale fix, roboplot default left margin change. Relating to a plotly bug, a change to default hovermode on some bar charts to show hovertemplate on undrawn bars.

# roboplotr 1.3.2.1

* Robomap dependency fixes.

# roboplotr 1.3.2

* Initial work for robomap data contour maps. This provides a smoother transition and helps in visualizing general trends across regions.

# roboplotr 1.3.1

* Added plot-specific pattern control. NA-control for numerics in robotable. Robomap visualization improvements.

# roboplotr 1.3.0

* Initialized robomap, automated leaflet map creation. Robotable modebar and heatmap across numeric columns.

# roboplotr 1.2.6

* Robotable infobutton initial code, css fixes. Title omission possible for robotable.

# roboplotr 1.2.5

* Robotable navigation for length and filter css fix.

# roboplotr 1.2.4

* Robotable navigation for length and filter css fix.

# roboplotr 1.2.3

* Rewrite how robotables handle css.

# roboplotr 1.2.2

* Preliminary work for robotable plotly-like modebar, improved number formats, and logo support.

# roboplotr 1.2.1

* Commit fix for missing robotable files.

# roboplotr 1.2.0

* Added set_title for controlling for controlling if the plot title will be displayed on the plot. Plot title will still be on any png downloads.
* Fixed plot axis limit bug introduced in 1.0.0.
* Fixed border width control bug.
* Initialized automated DT table creation

# roboplotr 1.1.1

* Documentation fixes for plot caption and roboplot options.

# roboplotr 1.1.0 

* Added the possibility of using log scale on numeric axes.

# roboplotr 1.0.0

* Initialized versioning of roboplotr with the current stable version.
* Reworked how x-axis tickmarks are formatted for zoom levels for timeseries of different frequencies.
* Disabled x-axis range control when rangeslider is used.

# roboplotr 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
