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
