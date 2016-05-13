#Configuration options
#warning message to display when user interface is still rendering. 
warningRender = "User interface has not finished rendering, please wait."

yourStr <-   '$(document).ready(function(){
                      $("#btn-1")
.popover({html: true,
trigger: "hover"
});
});   
' 
yourStr3 <-   '$(document).ready(function(){
                      $("#btn-3")
.popover({html: true,
trigger: "hover"
});
});   
' 
yourStr2 <-   '$(document).ready(function(){
                      $("#btn-2").popover({
                        html: true, trigger: "hover"});})'