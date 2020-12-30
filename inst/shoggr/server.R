function(input, output, session) {
  pickedSimu <- simuPickerServer("sidebarSimuPicker", session)
  
  observeEvent(pickedSimu(), {
    simuMonitorServer("simuMonitor", pickedSimu())
  })
  
  serverMonitorServer("serverMonitor")
  
  observeEvent(input$refreshPage, {
    session$reload()
  })
}
