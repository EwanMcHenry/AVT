ati_table_to_dt <- function(summary_table, caption.text, scroll_height = "400px", col.summary = "UK total" , row.summary = "Any record"){
  # dplyr::select(Sum = Sum, everything())
  colnames(summary_table)[dim(summary_table)[2]] <- col.summary
  rownames(summary_table)[nrow(summary_table)] <- row.summary
  
  datatable(summary_table, 
            caption = caption.text,
            extensions = 'Buttons', 
            options = list(
              dom = 'Bfrtip',  # Show buttons, filtering, etc.
              buttons = list(
                'copy', 'excel'  # Export options
              ),
              paging = F,  # Enable pagination
              scrollY = scroll_height,  # Enable vertical scrolling
              searching = TRUE,  # Enable column search/filter
              ordering = TRUE,  # Enable column sorting
              pageLength = 10,  # Number of rows per page
              lengthMenu = c(10, 25, 50, 100),  # Options for number of rows
              rowCallback = JS(
                glue("
                  function(row, data, index) {{
                    if (data[0] == '{row.summary}') {{
                      $(row).css({{'background-color': '#f9f9f9', 'font-weight': 'bold'}});
                    }}
                  }}
                ")
              ),  # Style the summary row dynamically
              columnDefs = list(
                list(targets = 0, className = "dt-center")  # Align the first column text to the center
              )
            )
  ) #%>%
    # htmlwidgets::onRender(
    #   "function(el, x) {
    #   $('td:nth-child(5), th:nth-child(5)').css({'border-right': '2px solid black'});  // Add vertical line to the right of the first column
    # }"
    # )
  
}
