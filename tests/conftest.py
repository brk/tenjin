import cli_subcommands

pytest_plugins = "10j_pytest_fixtures"


def pytest_sessionstart(session):
    """Called after the Session object has been created and before performing collection and entering the run test loop."""
    cli_subcommands.do_build_star()
