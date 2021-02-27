function(input, output, session) {
  settingsInput <- settingsMenuServer("settingsMenu")
  topout <- serverMonitorServer("serverMonitor", settingsInput)
  pickedSimu <- simuPickerServer("sidebarSimuPicker", session, topout)

  observeEvent(pickedSimu(), {
    simuMonitorServer("simuMonitor", pickedSimu(), topout, settingsInput)
  })

  observeEvent(input$refreshPage, {
    session$reload()
  })
}
