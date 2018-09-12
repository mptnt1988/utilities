# copy this to ~/.ipython/profile_default/startup
import os


def setup_conda_env_shopee_integration():
    from providers.backend.backend_connector import BackEndConnector
    global BackEndConnector


if os.environ['CONDA_DEFAULT_ENV'] == 'shopee-integration':
    setup_conda_env_shopee_integration()

