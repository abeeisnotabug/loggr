function(input, output, session) {
  settingsInput <- settingsMenuServer("settingsMenu")
  topout <- serverMonitorServer("serverMonitor", settingsInput)
  pickedSimu <- simuPickerServer("sidebarSimuPicker", session, topout)
  
  observeEvent(pickedSimu(), {
    simuMonitorServer("simuMonitor", pickedSimu(), topout)
  })
  
  observeEvent(input$refreshPage, {
    session$reload()
  })
}
