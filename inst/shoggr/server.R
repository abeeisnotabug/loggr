function(input, output, session) {
  pickedSimu <- simuPickerServer("sidebarSimuPicker", session)
  topout <- serverMonitorServer("serverMonitor")
  
  observeEvent(pickedSimu(), {
    simuMonitorServer("simuMonitor", pickedSimu(), topout)
  })
  
  observeEvent(input$refreshPage, {
    session$reload()
  })
}
