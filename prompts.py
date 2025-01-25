#!/usr/bin/env python
"""Generate prompts for the audit study."""
import glob
import os
import sqlite3
from typing import Tuple

################################################################################
# Interview-based prompts


def generate_redacted(interview_id: int) -> Tuple[str, str]:
    """Generate basic prompt for real redacted materials."""
    with open(os.path.join("resumes", "redacted", f"{interview_id}.txt"), "r") as f:
        resume = f.read()

    questions = []
    for question in glob.glob(
        os.path.join("questions", "redacted", f"{interview_id}_*.txt")
    ):
        with open(question, "r") as f:
            questions.append(f.read())

    # Get the system message
    with open(os.path.join("system_messages", "base.txt"), "r") as f:
        system_message = f.read()

    # Get the prompt
    with open(os.path.join("prompts", "base.txt"), "r") as f:
        prompt = f.read()

    # Format the prompt and add the resume and questions
    prompt = prompt + resume + "\n\n" + "\n\n".join(questions)

    return system_message, prompt


def generate_unredacted(interview_id: int) -> Tuple[str, str]:
    """Generate basic prompt for real unredacted materials."""
    with open(os.path.join("resumes", "raw", f"{interview_id}.txt"), "r") as f:
        resume = f.read()

    questions = []
    for question in glob.glob(
        os.path.join("questions", "unredacted", f"{interview_id}_*.txt")
    ):
        with open(question, "r") as f:
            questions.append(f.read())

    # Get the system message
    with open(os.path.join("system_messages", "base.txt"), "r") as f:
        system_message = f.read()

    # Get the prompt
    with open(os.path.join("prompts", "base.txt"), "r") as f:
        prompt = f.read()

    # Format the prompt and add the resume and questions
    prompt = prompt + resume + "\n\n" + "\n\n".join(questions)

    return system_message, prompt


INTERVIEW_PROMPT_GENERATORS = {
    "redacted": generate_redacted,
    "unredacted": generate_unredacted,
}


################################################################################
# Persona-based prompts


def generate_base(persona: dict) -> Tuple[str, str]:
    """Generate basic prompt."""
    # Get the resume and questions
    with open(
        os.path.join("resumes", "redacted", f"{persona['interview_id']}.txt"), "r"
    ) as f:
        resume = f.read()

    questions = []
    for question in glob.glob(
        os.path.join("questions", "redacted", f"{persona['interview_id']}_*.txt")
    ):
        with open(question, "r") as f:
            questions.append(f.read())

    # Get the system message
    with open(os.path.join("system_messages", "base.txt"), "r") as f:
        system_message = f.read()

    # Get the prompt
    with open(os.path.join("prompts", "base.txt"), "r") as f:
        prompt = f.read()

    # Format the prompt and add the resume and questions
    prompt = (
        prompt.format(**persona)
        + resume.format(**persona)
        + "\n\n"
        + "\n\n".join([q.format(**persona) for q in questions])
    )

    return system_message, prompt


def generate_no_scratch(persona: dict) -> Tuple[str, str]:
    """Generate prompt with no scratchpad."""
    # Get the resume and questions
    with open(
        os.path.join("resumes", "redacted", f"{persona['interview_id']}.txt"), "r"
    ) as f:
        resume = f.read()

    questions = []
    for question in glob.glob(
        os.path.join("questions", "redacted", f"{persona['interview_id']}_*.txt")
    ):
        with open(question, "r") as f:
            questions.append(f.read())

    # Get the system message
    with open(os.path.join("system_messages", "no_scratch.txt"), "r") as f:
        system_message = f.read()

    # Get the prompt
    with open(os.path.join("prompts", "no_scratch.txt"), "r") as f:
        prompt = f.read()

    # Format the prompt and add the resume and questions
    prompt = (
        prompt.format(**persona)
        + resume.format(**persona)
        + "\n\n"
        + "\n\n".join([q.format(**persona) for q in questions])
    )

    return system_message, prompt


def generate_no_trascripts(persona: dict) -> Tuple[str, str]:
    """Generate a prompt without transcripts."""
    # Get the resume
    with open(
        os.path.join("resumes", "redacted", f"{persona['interview_id']}.txt"), "r"
    ) as f:
        resume = f.read()

    # Get the system message
    with open(os.path.join("system_messages", "base.txt"), "r") as f:
        system_message = f.read()

    # Get the prompt
    with open(os.path.join("prompts", "no_transcripts.txt"), "r") as f:
        prompt = f.read()

    # Format the prompt and add the resume
    prompt = prompt.format(**persona) + resume.format(**persona)

    return system_message, prompt


def generate_other_district(persona: dict) -> Tuple[str, str]:
    """Generate prompt with other district."""
    other_district = {
        "school_district": "Kanawa County Schools",
        "school_city": "Charleston",
        "school_state": "West Virginia",
    }

    # Get the resume and questions
    with open(
        os.path.join("resumes", "redacted", f"{persona['interview_id']}.txt"), "r"
    ) as f:
        resume = f.read()

    questions = []
    for question in glob.glob(
        os.path.join("questions", "modified", f"{persona['interview_id']}_*.txt")
    ):
        with open(question, "r") as f:
            questions.append(f.read())

    # Get the system message
    with open(os.path.join("system_messages", "base.txt"), "r") as f:
        system_message = f.read()

    # Get the prompt
    with open(os.path.join("prompts", "other_district.txt"), "r") as f:
        prompt = f.read()

    # Format the prompt and add the resume and questions
    prompt = (
        prompt.format(**persona, **other_district)
        + resume.format(**persona, **other_district)
        + "\n\n"
        + "\n\n".join([q.format(**persona, **other_district) for q in questions])
    )

    return system_message, prompt


def generate_eeoc_guidance(persona: dict) -> Tuple[str, str]:
    """Generate prompt with EEOC guidance."""
    # Get the resume and questions
    with open(
        os.path.join("resumes", "redacted", f"{persona['interview_id']}.txt"), "r"
    ) as f:
        resume = f.read()

    questions = []
    for question in glob.glob(
        os.path.join("questions", "redacted", f"{persona['interview_id']}_*.txt")
    ):
        with open(question, "r") as f:
            questions.append(f.read())

    # Get the system message
    with open(os.path.join("system_messages", "base.txt"), "r") as f:
        system_message = f.read()

    # Get the prompt
    with open(os.path.join("prompts", "eeoc_guidance.txt"), "r") as f:
        prompt = f.read()

    # Format the prompt and add the resume and questions
    prompt = (
        prompt.format(**persona)
        + resume.format(**persona)
        + "\n\n"
        + "\n\n".join([q.format(**persona) for q in questions])
    )

    return system_message, prompt


def generate_manipulation_check(persona: dict) -> Tuple[str, str]:
    """Generate prompt with manipulation check."""
    # Get the resume and questions
    with open(
        os.path.join("resumes", "redacted", f"{persona['interview_id']}.txt"), "r"
    ) as f:
        resume = f.read()

    questions = []
    for question in glob.glob(
        os.path.join("questions", "redacted", f"{persona['interview_id']}_*.txt")
    ):
        with open(question, "r") as f:
            questions.append(f.read())

    # Get the system message
    with open(os.path.join("system_messages", "manipulation_check.txt"), "r") as f:
        system_message = f.read()

    # Get the prompt
    with open(os.path.join("prompts", "manipulation_check.txt"), "r") as f:
        prompt = f.read()

    # Format the prompt and add the resume and questions
    prompt = (
        prompt.format(**persona)
        + resume.format(**persona)
        + "\n\n"
        + "\n\n".join([q.format(**persona) for q in questions])
    )

    return system_message, prompt


def generate_variants(variant: int) -> callable:
    """Generate prompt with variants."""

    def generate_variant(persona: dict) -> Tuple[str, str]:
        # Get the resume and questions
        with open(
            os.path.join("resumes", "redacted", f"{persona['interview_id']}.txt"), "r"
        ) as f:
            resume = f.read()

        questions = []
        for question in glob.glob(
            os.path.join("questions", "redacted", f"{persona['interview_id']}_*.txt")
        ):
            with open(question, "r") as f:
                questions.append(f.read())

        # Get the system message
        with open(os.path.join("system_messages", f"variant_{variant}.txt"), "r") as f:
            system_message = f.read()

        # Get the prompt
        with open(os.path.join("prompts", f"variant_{variant}.txt"), "r") as f:
            prompt = f.read()

        # Format the prompt and add the resume and questions
        prompt = (
            prompt.format(**persona)
            + resume.format(**persona)
            + "\n\n"
            + "\n\n".join([q.format(**persona) for q in questions])
        )

        return system_message, prompt

    return generate_variant


PERSONA_PROMPT_GENERATORS = {
    "base": generate_base,
    "no_scratch": generate_no_scratch,
    "no_transcripts": generate_no_trascripts,
    "other_district": generate_other_district,
    "eeoc_guidance": generate_eeoc_guidance,
    "manipulation_check": generate_manipulation_check,
    **{f"variant_{i}": generate_variants(i) for i in range(4)},
}


if __name__ == "__main__":
    with sqlite3.connect("data.db") as conn:
        conn.row_factory = sqlite3.Row
        cur = conn.cursor()

        # For each experiment type, get the interview ids with no corresponding prompts
        for experiment_type, generator in INTERVIEW_PROMPT_GENERATORS.items():
            cur.execute(
                """
                SELECT interviews.interview_id FROM interviews
                LEFT JOIN (
                    SELECT * FROM prompts WHERE experiment_type = :experiment_type
                ) AS prompts
                ON interviews.interview_id = prompts.interview_id
                WHERE prompts.interview_id IS NULL
                AND interviews.in_study;
                """,
                {"experiment_type": experiment_type},
            )
            interview_ids = cur.fetchall()

            print(f"Generating {len(interview_ids)} prompts for {experiment_type}.")
            for i, interview_id in enumerate(interview_ids):
                print(f"\rGenerating prompt {i + 1} / {len(interview_ids)}", end="")
                if i == len(interview_ids) - 1:
                    print()
                system_message, prompt = generator(interview_id[0])
                cur.execute(
                    """
                    INSERT INTO prompts (
                        interview_id, system_message, prompt, experiment_type
                    ) VALUES (
                        :interview_id, :system_message, :prompt, :experiment_type
                    )
                    """,
                    {
                        "interview_id": interview_id[0],
                        "system_message": system_message,
                        "prompt": prompt,
                        "experiment_type": experiment_type,
                    },
                )
                conn.commit()

        # For each experiment type, get the persona ids with no corresponding prompts
        for experiment_type, generator in PERSONA_PROMPT_GENERATORS.items():
            cur.execute(
                """
                SELECT personas.* FROM personas
                LEFT JOIN (
                    SELECT * FROM prompts WHERE experiment_type = :experiment_type
                ) AS prompts
                ON personas.persona_id = prompts.persona_id
                WHERE prompts.persona_id IS NULL
                """,
                {"experiment_type": experiment_type},
            )
            personas = cur.fetchall()

            print(f"Generating {len(personas)} prompts for {experiment_type}.")
            for i, persona in enumerate(personas):
                print(f"\rGenerating prompt {i + 1} / {len(personas)}", end="")
                if i == len(personas) - 1:
                    print()
                system_message, prompt = generator(persona)
                cur.execute(
                    """
                        INSERT INTO prompts (
                            interview_id, persona_id, system_message, prompt,
                            experiment_type
                        ) VALUES (
                            :interview_id, :persona_id, :system_message,
                            :prompt, :experiment_type
                        )
                        """,
                    {
                        "interview_id": persona["interview_id"],
                        "persona_id": persona["persona_id"],
                        "system_message": system_message,
                        "prompt": prompt,
                        "experiment_type": experiment_type,
                    },
                )
                conn.commit()
