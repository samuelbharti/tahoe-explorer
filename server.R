# App server. Each registered page mounts its own module server(s).
function(input, output, session) {
  mount_page_servers()
}
