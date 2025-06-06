import hashlib


def compute_sha256(file_path) -> str | None:
    """
    Compute the SHA256 hash of a file.

    Args:
        file_path (str): Path to the file

    Returns:
        str: The SHA256 hash as a hexadecimal string
    """
    sha256_hash = hashlib.sha256()

    try:
        with open(file_path, "rb") as f:
            # Read the file in chunks to handle large files efficiently
            for byte_block in iter(lambda: f.read(8192), b""):
                sha256_hash.update(byte_block)
        return sha256_hash.hexdigest()
    except Exception as e:
        print(f"Error computing hash: {e}")
        return None


if __name__ == "__main__":
    import sys

    print(compute_sha256(sys.argv[1]), "\t", sys.argv[1])
