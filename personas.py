#!usr/bin/env python3
"""Add personae to the database."""

import random
import sqlite3

################################################################################

COLLEGES = [
    # Rank: 133
    # Pct. Asian: 24%
    # Pct. Black: 11%
    # Pct. Hispanic: 37%
    # Pct. White: 18%
    # SAT: 1160-1330
    {"college": "University of Houston", "city": "Houston", "state": "TX"},
    # Rank: 236
    # Pct. Asian: 14%
    # Pct. Black: 15%
    # Pct. Hispanic: 38%
    # Pct. White: 25%
    # SAT: 1000-1240
    {"college": "University of Texas at Arlington", "city": "Arlington", "state": "TX"},
    # Rank: 260
    # Pct. Asian: 8%
    # Pct. Black: 15%
    # Pct. Hispanic: 29%
    # Pct. White: 38%
    # SAT: 1000-1240
    {"college": "University of North Texas", "city": "Denton", "state": "TX"},
]

NAMES = {
    "female": {
        "Asian": [
            {"first_name": "Myong Hee", "last_name": "Shin"},
            {"first_name": "Thao", "last_name": "Tran"},
            {"first_name": "Hoa", "last_name": "Dinh"},
            {"first_name": "Vinodhini", "last_name": "Selva Kumar"},
            {"first_name": "Hong-Nga", "last_name": "Nguyen"},
            {"first_name": "Oanh", "last_name": "Nguyen"},
            {"first_name": "Yanbing", "last_name": "Li"},
            {"first_name": "Ngoc", "last_name": "Ngo"},
            {"first_name": "Kimanh", "last_name": "Dang"},
            {"first_name": "Mina", "last_name": "Kim"},
            {"first_name": "Anitha", "last_name": "Madhavaram"},
            {"first_name": "Thoa", "last_name": "Le"},
            {"first_name": "Chunyu", "last_name": "Liu"},
            {"first_name": "Sumayyah", "last_name": "Khan"},
            {"first_name": "Li", "last_name": "Zhu"},
            {"first_name": "Dipali", "last_name": "Patel"},
            {"first_name": "Amita", "last_name": "Patel"},
            {"first_name": "Heminaben", "last_name": "Patel"},
            {"first_name": "Pallavi", "last_name": "Sharma"},
            {"first_name": "Shrika", "last_name": "Renganathan"},
        ],
        "Black": [
            {"first_name": "Da'nashia", "last_name": "Sloan"},
            {"first_name": "Ka'dashia", "last_name": "Maxwell"},
            {"first_name": "Lashawna", "last_name": "Moore"},
            {"first_name": "Myeshia", "last_name": "Mcintyre"},
            {"first_name": "Atiya", "last_name": "Petteway"},
            {"first_name": "Lashondra", "last_name": "Gordon"},
            {"first_name": "Lashae", "last_name": "Lovett-Veal"},
            {"first_name": "Tomisha", "last_name": "Grimes"},
            {"first_name": "Dedria", "last_name": "Merritt"},
            {"first_name": "Tanekia", "last_name": "Riley"},
            {"first_name": "L'tahnya", "last_name": "Canty"},
            {"first_name": "Ca'niyah", "last_name": "Moore-Fleming"},
            {"first_name": "Lakeetha", "last_name": "Blakeney"},
            {"first_name": "Roshawnda", "last_name": "Pittman"},
            {"first_name": "Lashawnda", "last_name": "Burch"},
            {"first_name": "Lashanda", "last_name": "Williams"},
            {"first_name": "Nyeshia", "last_name": "Carswell"},
            {"first_name": "Laneshia", "last_name": "Coleman"},
            {"first_name": "Latrecia", "last_name": "Barnes Mangum"},
            {"first_name": "Natosha", "last_name": "Martin"},
        ],
        "Hispanic": [
            {"first_name": "Daisy", "last_name": "Rodriguez-Pereda"},
            {"first_name": "Yocelyn", "last_name": "Almanza-Figueroa"},
            {"first_name": "Yeralis", "last_name": "Morales Fernandez"},
            {"first_name": "Yesenia", "last_name": "Vasquez"},
            {"first_name": "Anahi", "last_name": "Santander Hernandez"},
            {"first_name": "Mariana", "last_name": "Aguilar-Rosas"},
            {"first_name": "Lucero", "last_name": "Hernandez-Hernandez"},
            {"first_name": "Myrian", "last_name": "Delgado"},
            {"first_name": "Natalia", "last_name": "Prudencio Mendoza"},
            {"first_name": "Nathaly", "last_name": "Estrada Lopez"},
            {"first_name": "Yaquelin", "last_name": "Tejada Peraza"},
            {"first_name": "Citlali", "last_name": "Dominguez De La Luz"},
            {"first_name": "Katerin", "last_name": "Molina-Reyes"},
            {"first_name": "Irma", "last_name": "Najera Acevedo"},
            {"first_name": "Jackelin", "last_name": "Garcia"},
            {"first_name": "Anastasia", "last_name": "Francisco Mateo"},
            {"first_name": "Shalymar", "last_name": "Perez Alejandro"},
            {"first_name": "Maria", "last_name": "Uribe"},
            {"first_name": "Carolina", "last_name": "Rangel Lara"},
            {"first_name": "Evelin", "last_name": "Dominguez"},
        ],
        "White": [
            {"first_name": "Susan", "last_name": "Strysko"},
            {"first_name": "Katharine", "last_name": "Tempelaar-Lietz"},
            {"first_name": "Valerie", "last_name": "Zombek"},
            {"first_name": "Laura", "last_name": "Zellers"},
            {"first_name": "Melinda", "last_name": "Waegerle"},
            {"first_name": "Judith", "last_name": "Schultz"},
            {"first_name": "Lara", "last_name": "Zeigler"},
            {"first_name": "Eileen", "last_name": "Rothberg"},
            {"first_name": "Amanda", "last_name": "Highberg"},
            {"first_name": "Audrey", "last_name": "Hoffman"},
            {"first_name": "Laurie", "last_name": "Baken"},
            {"first_name": "Karen", "last_name": "Ihrig"},
            {"first_name": "Jennifer", "last_name": "Schultz"},
            {"first_name": "Lureen", "last_name": "Vanderkuyl"},
            {"first_name": "Lucinda", "last_name": "Dillinger"},
            {"first_name": "Morghan", "last_name": "Eckenfels"},
            {"first_name": "Katherine", "last_name": "Coen"},
            {"first_name": "Kathryn", "last_name": "Stolba"},
            {"first_name": "Linda", "last_name": "Borkowski"},
            {"first_name": "Meredith", "last_name": "Schultz"},
        ],
    },
    "male": {
        "Asian": [
            {"first_name": "Gaurav", "last_name": "Bhagirath"},
            {"first_name": "Dailen", "last_name": "Luangpakdy"},
            {"first_name": "Za", "last_name": "Luai"},
            {"first_name": "Bhargav", "last_name": "Vaduri"},
            {"first_name": "Naveen", "last_name": "Natesh"},
            {"first_name": "Ambarish", "last_name": "Mhaskar"},
            {"first_name": "Satyanarayan", "last_name": "Panchangam Venkata"},
            {"first_name": "Diwash", "last_name": "Bhusal"},
            {"first_name": "Sanjay", "last_name": "Swaninathan"},
            {"first_name": "Ali", "last_name": "Bawangoanwala"},
            {"first_name": "Kalyan", "last_name": "Venkatraj"},
            {"first_name": "Yogesh", "last_name": "Tekwani"},
            {"first_name": "Neng", "last_name": "Vang"},
            {"first_name": "Nihit", "last_name": "Chavan"},
            {"first_name": "Xingwu", "last_name": "Wang"},
            {"first_name": "Krishna", "last_name": "Pathak"},
            {"first_name": "Vinh", "last_name": "Bui"},
            {"first_name": "Shivam", "last_name": "Patel"},
            {"first_name": "Veng", "last_name": "Chang"},
            {"first_name": "Shailesh", "last_name": "Pancholi"},
        ],
        "Black": [
            {"first_name": "Isiah", "last_name": "Wright"},
            {"first_name": "Dontayious", "last_name": "Staton"},
            {"first_name": "Dontay", "last_name": "Dunton"},
            {"first_name": "Hezekiah", "last_name": "James"},
            {"first_name": "Dajhon", "last_name": "Prince"},
            {"first_name": "Xaviar", "last_name": "Wright"},
            {"first_name": "Tavarrel", "last_name": "Whitaker"},
            {"first_name": "Jahlil", "last_name": "Anderson"},
            {"first_name": "Zay-Quan", "last_name": "Price"},
            {"first_name": "Isaiah", "last_name": "Robinson"},
            {"first_name": "Khalil", "last_name": "Cooke"},
            {"first_name": "Sayquawn", "last_name": "Carr"},
            {"first_name": "Zevonte", "last_name": "Stephens"},
            {"first_name": "Davon", "last_name": "Underwood"},
            {"first_name": "Juquan", "last_name": "Edmonds"},
            {"first_name": "Keandre", "last_name": "Marshall"},
            {"first_name": "Montell", "last_name": "Gwinn"},
            {"first_name": "Davonte", "last_name": "Belle"},
            {"first_name": "Junious", "last_name": "Harris"},
            {"first_name": "Montayvius", "last_name": "Joyner"},
        ],
        "Hispanic": [
            {"first_name": "Luiz", "last_name": "Gutierrez Rodriguez"},
            {"first_name": "Juan", "last_name": "Quintanilla"},
            {"first_name": "Luis", "last_name": "Ruberte Vazquez"},
            {"first_name": "Carlos", "last_name": "Hurtado Tovar"},
            {"first_name": "Rodrigo", "last_name": "Arellano-Hernandez"},
            {"first_name": "Jesus", "last_name": "Hernandez Carranza"},
            {"first_name": "Sergio", "last_name": "Calvillo-Santoyo"},
            {"first_name": "Rogelio", "last_name": "Hurtado"},
            {"first_name": "Luis Fernando", "last_name": "Gonzales-Zunija"},
            {"first_name": "Jean Luis", "last_name": "Sanchez Capellan"},
            {"first_name": "Gustavo", "last_name": "Villegas"},
            {"first_name": "Gabriel", "last_name": "Ochoa Hernandez"},
            {"first_name": "Jose", "last_name": "Ortiz"},
            {"first_name": "Rigoberto", "last_name": "Aguilar"},
            {"first_name": "Fabio", "last_name": "Diaz Corniel"},
            {"first_name": "Juan Carlos", "last_name": "Berrios Colon"},
            {"first_name": "Manuel Jesus", "last_name": "Diaz Gutierrez"},
            {"first_name": "Roberto", "last_name": "Cortez"},
            {"first_name": "Miguelangel", "last_name": "Ruiz"},
            {"first_name": "Pedro", "last_name": "Hernandez"},
        ],
        "White": [
            {"first_name": "Zachary", "last_name": "Piephoff"},
            {"first_name": "Duane", "last_name": "Scholz"},
            {"first_name": "Timothy", "last_name": "Boehm"},
            {"first_name": "Benjamin", "last_name": "Fichter"},
            {"first_name": "William", "last_name": "Von Hoerling"},
            {"first_name": "Stephen", "last_name": "Wernsing"},
            {"first_name": "Richard", "last_name": "Reichow"},
            {"first_name": "Steven", "last_name": "Olczak"},
            {"first_name": "David", "last_name": "Von Kolnitz"},
            {"first_name": "Samuel", "last_name": "Zweifel"},
            {"first_name": "Robert", "last_name": "Ohern"},
            {"first_name": "Kenneth", "last_name": "Krzyzewski"},
            {"first_name": "Johnathon", "last_name": "Bohnert"},
            {"first_name": "Jacob", "last_name": "Schroeder"},
            {"first_name": "Thomas", "last_name": "Schafer"},
            {"first_name": "Noah", "last_name": "Gerow"},
            {"first_name": "Gary", "last_name": "Gerlach"},
            {"first_name": "Joseph", "last_name": "Zielazinski"},
            {"first_name": "John", "last_name": "Abbruzzese"},
            {"first_name": "Jeffrey", "last_name": "Bechtel"},
        ],
    },
}

################################################################################


def generate_persona(race: str, gender: str) -> dict:
    """Generate a persona."""
    # Generate pronouns and title
    if gender == "female":
        nominative = "she"
        genitive = "hers"
        oblique = "her"
        title = "Ms."
    else:
        nominative = "he"
        genitive = "his"
        oblique = "him"
        title = "Mr."

    # Generate name
    name = random.choice(NAMES[gender][race])

    # Draw a college at random
    college = random.choice(COLLEGES)

    # Return the persona
    persona = {
        "race": race,
        "gender": gender,
        "first_name": name["first_name"],
        "last_name": name["last_name"],
        "nominative": nominative,
        "genitive": genitive,
        "oblique": oblique,
        "title": title,
        "college": college["college"],
        "city": college["city"],
        "state": college["state"],
    }
    return persona


if __name__ == "__main__":
    # Connect to the database
    conn = sqlite3.connect("data.db")

    with sqlite3.connect("data.db") as conn:
        cur = conn.cursor()
        # Left join interviews and persona to get interviews that don't have a persona
        interviews = cur.execute(
            """
            SELECT interviews.interview_id
            FROM interviews
            LEFT JOIN personas
            ON interviews.interview_id = personas.interview_id
            WHERE personas.interview_id IS NULL
            AND interviews.in_study;
            """
        ).fetchall()

    for interview in interviews:
        for race in ["Asian", "Black", "Hispanic", "White"]:
            for gender in ["female", "male"]:
                persona = generate_persona(race, gender)

                with sqlite3.connect("data.db") as conn:
                    cur = conn.cursor()
                    cur.execute(
                        """
                        INSERT INTO personas (
                            interview_id,
                            first_name,
                            last_name,
                            race,
                            gender,
                            title,
                            college,
                            city,
                            state,
                            nominative,
                            genitive,
                            oblique
                        ) VALUES (
                            :interview_id,
                            :first_name,
                            :last_name,
                            :race,
                            :gender,
                            :title,
                            :college,
                            :city,
                            :state,
                            :nominative,
                            :genitive,
                            :oblique
                        );
                        """,
                        {"interview_id": interview[0], **persona},
                    )
                    conn.commit()
