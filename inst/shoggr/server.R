function(input, output, session) {
  logFolderContents <- checkLogFolderContentServer("checkLogFolder")

  # simuMonitorModuleServer("simuMonitorModule")
  simuPickerModuleServer("sidebarSimuPicker", logFolderContents)
  serverMonitorModuleServer("serverMonitorModule")
}
