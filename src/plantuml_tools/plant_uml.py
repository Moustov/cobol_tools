from datetime import datetime, timedelta
import datetime


def get_bank_holidays() -> str:
    bank_holidays = """
    2024/05/01 is colored in Lavender/LightBlue 
    2024/05/08 is colored in Lavender/LightBlue 
    2024/05/09 is colored in Lavender/LightBlue 
    2024/05/20 is colored in Lavender/LightBlue 
    2024/07/14 is colored in Lavender/LightBlue 
    """
    return bank_holidays


def get_epic_separator_style() -> str:
    gantt_string_res = """
    <style>
    ganttDiagram {
        separator {
            LineColor red
            BackGroundColor Orange
            FontSize 16
            FontStyle bold
            FontColor purple
        }
    }
    </style>    
    """
    return gantt_string_res


def get_us_separator_style() -> str:
    gantt_string_res = """
    <style>
    ganttDiagram {
        separator {
            LineColor black
            BackGroundColor While
            FontSize 10
            FontStyle
            FontColor black
        }
    }
    </style>    
    """
    return gantt_string_res


def get_nombre_jours_ouvres(d1: str, d2: str) -> int:
    # Définir les dates de début et de fin
    date_debut = datetime.datetime.strptime(d1, "%Y/%m/%d")
    date_fin = datetime.datetime.strptime(d2, "%Y/%m/%d")
    if d2 >= d1:
        d_fin = date_fin
        jour_actuel = date_debut
    else:
        d_fin = date_debut
        jour_actuel = date_fin
    # Initialiser le compteur de jours
    nombre_de_jours = 0
    # Parcourir chaque jour entre les deux dates
    while jour_actuel <= d_fin:
        # Si le jour n'est ni un samedi (5) ni un dimanche (6), on l'ajoute au compteur
        if jour_actuel.weekday() < 5:
            nombre_de_jours += 1
        # Passer au jour suivant
        jour_actuel += datetime.timedelta(days=1)
    if d2 > d1:
        return nombre_de_jours
    else:
        return -nombre_de_jours


def get_delta_jours_dates(d_min, d_max) -> int:
    date_debut = datetime.datetime.strptime(d_min, "%Y/%m/%d")
    date_fin = datetime.datetime.strptime(d_max, "%Y/%m/%d")
    delta = date_fin - date_debut
    return delta.days


def get_date_plus_duree_jira(derniere_action: dict, pourcent: float) -> str:
    """
    todo : ralonger la tache pour que l'avancement arrive au dernier jour pointé
    prendre en compte la date de la dernière action + la durée dans le worklog
    :param pourcent:
    :param derniere_action:
    :return:
    """
    date_str = derniere_action["started"]
    # Convertir la chaîne de caractères en objet datetime
    date = datetime.datetime.strptime(date_str, "%Y-%m-%dT%H:%M:%S.%f%z")
    jours = int(derniere_action["timeSpentSeconds"] / 3600.0 / 8.0)
    # Ajouter les heures
    date_modifiee_str = get_date_plus_x_jours_ouvres(date.strftime("%Y/%m/%d"), jours)
    # print(date_str, jours, date_modifiee_str, file=sys.stderr)
    return date_modifiee_str


def get_date_plus_x_jours_ouvres(date_debut: str, x: int) -> str:
    date_debut = datetime.datetime.strptime(date_debut, "%Y/%m/%d")
    # Initialiser le compteur de jours
    nombre_de_jours = 0
    # Parcourir chaque jour entre les deux dates
    while nombre_de_jours <= x:
        # Si le jour n'est ni un samedi (5) ni un dimanche (6), on l'ajoute au compteur
        if date_debut.weekday() < 5:
            nombre_de_jours += 1
        # Passer au jour suivant
        date_debut += datetime.timedelta(days=1)
    return date_debut.strftime("%Y/%m/%d")


def get_date_plus_x_jours(date_debut: str, x: int) -> str:
    """

    :param date_debut: "AAAA/MM/JJ"
    :param x: nb jours
    :return:
    """
    date = datetime.datetime.strptime(date_debut, "%Y/%m/%d")
    date_modifiee = date + timedelta(days=x)
    # Convertir l'objet datetime en chaîne de caractères
    date_modifiee_str = date_modifiee.strftime("%Y/%m/%d")
    return date_modifiee_str

