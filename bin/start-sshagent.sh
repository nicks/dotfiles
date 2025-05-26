# https://sites.google.com/site/axusdev/tutorials/createsshkeysinmsys
# sshagent.sh: Start SSH agent (if needed) and set agent environment variables

# Be sure to "source" this script, to recieve the necessary variables:
# SSH_AUTH_SOCK: socket for the running SSH agent
# SSH_AGENT_PID: Process ID for the running SSH agent

# As always, make sure your .ssh directory is only readble by you!

SSH_ENV="$HOME/.ssh/environment"

function start_agent {
  echo "Initializing new SSH agent..."
  ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
  echo succeeded
  chmod 600 "${SSH_ENV}"
  . "${SSH_ENV}" > /dev/null
  ssh-add;
}

# Source SSH settings, if applicable
if [ -f "${SSH_ENV}" ]; then
  . "${SSH_ENV}" > /dev/null
  ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
    start_agent;
  }
else
  start_agent;
fi
