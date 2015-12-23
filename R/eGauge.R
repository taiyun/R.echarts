#' Gauge Charts
#'
#' ECharts style Gauge charts.
#'
#' @usage  eGauge(value, label = NULL, center = c(0.5,0.5), 
#'                radius = 0.75, angle = c(-45, 225), 
#'                range = c(0, 100),  percent = F, 
#'                size = NULL, theme = "default",opt=list(),...)
#'                 
#' @param value    value for Gauge Chart
#' @param center   center of the plot,(width,height) 0.5 means 50%
#' @param radius   radius of the plot
#' @param angle    start and end angle of the Gauge Chart
#' @param range    max and min of the Gauge Chart
#' @param percent  add "%" after the value or not
#' @param theme    theme of the Echarts
#' @return an htmlwidgets object which can be rendered by Browser.
#' @export
#' @examples
#' 
#' SuccessRate = 80
#' eGauge(SuccessRate)
#' eGauge(80,label = "Success Rate", 
#'        percent=T,theme="macarons")
#' eGauge(80,label = "Success Rate",angle = c(45,-45), 
#'          percent=T,theme="macarons")




eGauge = function(value, label = NULL,center = c(0.5,0.5), 
    radius = 0.75, angle = c(225, -45), range = c(0, 100),
    percent = F, size = NULL, showLabel=TRUE,
    theme = "default", title = NULL, subtitle = NULL, title.x = "center", title.y = "top",
    legend = TRUE, legend.x = "left", legend.y= "top", legend.orient="horizontal",
    toolbox = TRUE, toolbox.orient = "horizontal", toolbox.x = "right", toolbox.y = "top",
    dataView = FALSE, readOnly = TRUE, mark=TRUE, dataZoom=FALSE,
    tooltip = TRUE, tooltip.trigger="item", formatter="",
    calculable=FALSE, xlab = NULL, ylab=NULL, opt = list() ) {

        if(is.null(label)){
                label = deparse(substitute(value))
        }


        opt$title = tilteSet(title = title, subtitle=subtitle,
                             title.x = title.x, title.y = title.y)

        opt$calculable = calculableSet(calculable = calculable)
        opt$theme = themeSet(theme = theme)
        # opt$tooltip format, not open to user now.
        opt$tooltip = tooltipSet( tooltip=tooltip,trigger=tooltip.trigger,
                                  formatter = "{a} <br/>{b} : {c}%", islandFormatter="")

        datFrame = data.frame(value=value, name=label)
        datList = unname(lapply(split(datFrame, seq_len(nrow(datFrame))), as.list))


    	opt$series = list(
            list(
                name = "Gauge",
                type = "gauge",
                center = percentTrans(center),
                radius = percentTrans(radius),
            	  startAngle = angle[1],
            	  endAngle = angle[2],
                min = range[1],
                max = range[2],
                split = 10,
                data = datList
                )
            )


    	if(percent){
            opt$series[[1]]$detail$formatter = '{value}%'
        }

        opt$size = size

    	### output list format
    	chart = htmlwidgets::createWidget(
    		'echarts', opt, width = size[1], height = size[2], package = 'recharts'
    	)
    	chart
}


percentTrans = function(vec){
    paste0(vec*100,"%")
}
## percentTrans(c(1:2))
