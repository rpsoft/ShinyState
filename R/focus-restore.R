#' @keywords internal
focus_tracking_script <- function(ns) {
  root_id <- ns("ui")
  channel <- ns(".shinystate_editing")
  htmltools::singleton(
    shiny::tags$script(
      htmltools::HTML(
        sprintf(
          paste0(
            "(function(){var key='__shinystate_track_%s';",
            "if(window[key])return;window[key]=true;",
            "var rootId='%s',channel='%s';",
            "function isTypingElement(el){",
            "if(!el)return false;",
            "if(el.tagName==='TEXTAREA')return true;",
            "if(el.tagName==='INPUT'){",
            "var type=(el.type||'text').toLowerCase();",
            "return type==='text'||type==='search'||type==='email'||",
            "type==='password'||type==='url'||type==='number';",
            "}",
            "return false;",
            "}",
            "function isEditing(){",
            "var root=document.getElementById(rootId);",
            "var active=document.activeElement;",
            "return !!(root&&active&&root.contains(active)&&isTypingElement(active));",
            "}",
            "function notify(){",
            "Shiny.setInputValue(channel,isEditing(),{priority:'event'});",
            "}",
            "document.addEventListener('focusin',function(e){",
            "var root=document.getElementById(rootId);",
            "if(root&&root.contains(e.target))notify();",
            "},true);",
            "document.addEventListener('focusout',function(e){",
            "var root=document.getElementById(rootId);",
            "if(root&&root.contains(e.target))setTimeout(notify,0);",
            "},true);",
            "document.addEventListener('input',function(e){",
            "var root=document.getElementById(rootId);",
            "if(root&&root.contains(e.target)&&isTypingElement(e.target))notify();",
            "},true);",
            "})();"
          ),
          gsub("[^a-zA-Z0-9]", "_", root_id),
          root_id,
          channel
        )
      )
    )
  )
}

#' @keywords internal
wrap_component_ui <- function(ns, ui) {
  htmltools::tagList(
    ui,
    focus_tracking_script(ns)
  )
}
