import pandas as pd
import os


USECASE_SPEC = os.path.join("..", 'PHUSE CSS_2020_hackathon_AD usecase.xlsx')
AESI_SPEC = os.path.join("..", 'aes_of_interest.xlsx')

def load_usecase_specification(dataset):
    """
    Return the custom specs and codelist per
    """
    specification = USECASE_SPEC
    if os.path.exists(specification):
        ds = pd.read_excel(specification, dataset)
        cl = pd.read_excel(specification, "Codelist")
        return ds, cl
    return None, None



