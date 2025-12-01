import tomllib

import hermetic


def flags_for_all_cargo_workspace_packages(
    workspace_root: hermetic.Path,
) -> list[str]:
    flags = []
    packages = packages_for_cargo_workspace(workspace_root)
    for pkg in packages:
        flags.append("-p")
        flags.append(pkg)
    return flags


def packages_for_cargo_workspace(
    workspace_root: hermetic.Path,
) -> list[str]:
    cargo_toml_path = workspace_root / "Cargo.toml"
    ct = tomllib.load(cargo_toml_path.open("rb"))

    if "workspace" not in ct:
        if "package" not in ct:
            return []
        return [ct["package"]["name"]]

    member_names = ct["workspace"].get("members", [])
    if len(member_names) == 0:
        return [ct["package"]["name"]]

    package_names = []
    for member in member_names:
        member_ct = tomllib.load((workspace_root / member / "Cargo.toml").open("rb"))
        package_names.append(member_ct["package"]["name"])
    return package_names
