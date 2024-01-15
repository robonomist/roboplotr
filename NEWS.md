# roboplotr 1.4.0.1

* Added missing webshot2 dependency

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
