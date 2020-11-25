import os

fptaylor_exe = os.path.join(os.environ.get('FPTAYLOR_BASE', '..'), 'fptaylor')
errorbounds_path = os.environ.get('ERROR_BOUNDS', os.path.join('..', '..', 'ErrorBounds'))
