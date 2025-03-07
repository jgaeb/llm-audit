CREATE TABLE IF NOT EXISTS interviews (
    interview_id INTEGER PRIMARY KEY,
    race TEXT NOT NULL,
    gender TEXT NOT NULL,
    in_study BOOLEAN NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_interviews_in_study ON interviews(in_study);

CREATE TABLE IF NOT EXISTS personas (
    persona_id INTEGER PRIMARY KEY AUTOINCREMENT,
    interview_id INTEGER NOT NULL,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    race TEXT NOT NULL,
    gender TEXT NOT NULL,
    title TEXT NOT NULL,
    college TEXT NOT NULL,
    city TEXT NOT NULL,
    state TEXT NOT NULL,
    nominative TEXT NOT NULL,
    genitive TEXT NOT NULL,
    oblique TEXT NOT NULL,
    FOREIGN KEY (interview_id) REFERENCES interviews(interview_id)
);
CREATE INDEX IF NOT EXISTS idx_personas_interview_id ON personas(interview_id);

CREATE TABLE IF NOT EXISTS prompts (
    prompt_id INTEGER PRIMARY KEY AUTOINCREMENT,
    interview_id INTEGER NOT NULL,
    persona_id INTEGER,
    /*
    NOTE: Removed for public release
    prompt TEXT NOT NULL,
    system_message TEXT NOT NULL,
    */
    experiment_type TEXT NOT NULL,
    FOREIGN KEY (interview_id) REFERENCES interviews(interview_id),
    FOREIGN KEY (persona_id) REFERENCES personas(persona_id)
);
CREATE INDEX IF NOT EXISTS idx_prompts_interview_id ON prompts(interview_id);
CREATE INDEX IF NOT EXISTS idx_prompts_persona_id ON prompts(persona_id);
CREATE INDEX IF NOT EXISTS idx_prompts_experiment_type ON prompts(experiment_type);

CREATE TABLE IF NOT EXISTS requests (
    request_id INTEGER PRIMARY KEY AUTOINCREMENT,
    prompt_id INTEGER NOT NULL,
    model TEXT NOT NULL,
    /* NOTE: Removed for public release
    raw_response TEXT,
    */
    error BOOLEAN,
    error_message TEXT,
    timestamp DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (prompt_id) REFERENCES prompts(prompt_id)
);
CREATE INDEX IF NOT EXISTS idx_requests_model ON requests(model);
CREATE INDEX IF NOT EXISTS idx_requests_prompt_id ON requests(prompt_id);

CREATE TABLE IF NOT EXISTS ratings (
    parsed_id INTEGER PRIMARY KEY AUTOINCREMENT,
    request_id INTEGER NOT NULL,
    experience INTEGER,
    professionalism INTEGER,
    fit INTEGER,
    hire BOOLEAN,
    parsed BOOLEAN,
    error BOOLEAN NOT NULL,
    error_message TEXT,
    timestamp DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (request_id) REFERENCES requests(request_id)
);
CREATE INDEX IF NOT EXISTS idx_ratings_request_id ON ratings(request_id);

CREATE TABLE IF NOT EXISTS checks (
    parsed_id INTEGER PRIMARY KEY AUTOINCREMENT,
    request_id INTEGER NOT NULL,
    race TEXT,
    gender TEXT,
    parsed BOOLEAN,
    error BOOLEAN NOT NULL,
    error_message TEXT,
    timestamp DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (request_id) REFERENCES requests(request_id)
);
CREATE INDEX IF NOT EXISTS idx_checks_request_id ON checks(request_id);

CREATE TABLE IF NOT EXISTS embeddings (
  embedding_id INTEGER PRIMARY KEY AUTOINCREMENT,
  interview_id INTEGER NOT NULL,
  redacted BOOLEAN NOT NULL,
  resume BOOLEAN NOT NULL,
  X_0 REAL NOT NULL,
  X_1 REAL NOT NULL,
  X_2 REAL NOT NULL,
  X_3 REAL NOT NULL,
  X_4 REAL NOT NULL,
  X_5 REAL NOT NULL,
  X_6 REAL NOT NULL,
  X_7 REAL NOT NULL,
  X_8 REAL NOT NULL,
  X_9 REAL NOT NULL,
  X_10 REAL NOT NULL,
  X_11 REAL NOT NULL,
  X_12 REAL NOT NULL,
  X_13 REAL NOT NULL,
  X_14 REAL NOT NULL,
  X_15 REAL NOT NULL,
  X_16 REAL NOT NULL,
  X_17 REAL NOT NULL,
  X_18 REAL NOT NULL,
  X_19 REAL NOT NULL,
  X_20 REAL NOT NULL,
  X_21 REAL NOT NULL,
  X_22 REAL NOT NULL,
  X_23 REAL NOT NULL,
  X_24 REAL NOT NULL,
  X_25 REAL NOT NULL,
  X_26 REAL NOT NULL,
  X_27 REAL NOT NULL,
  X_28 REAL NOT NULL,
  X_29 REAL NOT NULL,
  X_30 REAL NOT NULL,
  X_31 REAL NOT NULL,
  X_32 REAL NOT NULL,
  X_33 REAL NOT NULL,
  X_34 REAL NOT NULL,
  X_35 REAL NOT NULL,
  X_36 REAL NOT NULL,
  X_37 REAL NOT NULL,
  X_38 REAL NOT NULL,
  X_39 REAL NOT NULL,
  X_40 REAL NOT NULL,
  X_41 REAL NOT NULL,
  X_42 REAL NOT NULL,
  X_43 REAL NOT NULL,
  X_44 REAL NOT NULL,
  X_45 REAL NOT NULL,
  X_46 REAL NOT NULL,
  X_47 REAL NOT NULL,
  X_48 REAL NOT NULL,
  X_49 REAL NOT NULL,
  X_50 REAL NOT NULL,
  X_51 REAL NOT NULL,
  X_52 REAL NOT NULL,
  X_53 REAL NOT NULL,
  X_54 REAL NOT NULL,
  X_55 REAL NOT NULL,
  X_56 REAL NOT NULL,
  X_57 REAL NOT NULL,
  X_58 REAL NOT NULL,
  X_59 REAL NOT NULL,
  X_60 REAL NOT NULL,
  X_61 REAL NOT NULL,
  X_62 REAL NOT NULL,
  X_63 REAL NOT NULL,
  X_64 REAL NOT NULL,
  X_65 REAL NOT NULL,
  X_66 REAL NOT NULL,
  X_67 REAL NOT NULL,
  X_68 REAL NOT NULL,
  X_69 REAL NOT NULL,
  X_70 REAL NOT NULL,
  X_71 REAL NOT NULL,
  X_72 REAL NOT NULL,
  X_73 REAL NOT NULL,
  X_74 REAL NOT NULL,
  X_75 REAL NOT NULL,
  X_76 REAL NOT NULL,
  X_77 REAL NOT NULL,
  X_78 REAL NOT NULL,
  X_79 REAL NOT NULL,
  X_80 REAL NOT NULL,
  X_81 REAL NOT NULL,
  X_82 REAL NOT NULL,
  X_83 REAL NOT NULL,
  X_84 REAL NOT NULL,
  X_85 REAL NOT NULL,
  X_86 REAL NOT NULL,
  X_87 REAL NOT NULL,
  X_88 REAL NOT NULL,
  X_89 REAL NOT NULL,
  X_90 REAL NOT NULL,
  X_91 REAL NOT NULL,
  X_92 REAL NOT NULL,
  X_93 REAL NOT NULL,
  X_94 REAL NOT NULL,
  X_95 REAL NOT NULL,
  X_96 REAL NOT NULL,
  X_97 REAL NOT NULL,
  X_98 REAL NOT NULL,
  X_99 REAL NOT NULL,
  X_100 REAL NOT NULL,
  X_101 REAL NOT NULL,
  X_102 REAL NOT NULL,
  X_103 REAL NOT NULL,
  X_104 REAL NOT NULL,
  X_105 REAL NOT NULL,
  X_106 REAL NOT NULL,
  X_107 REAL NOT NULL,
  X_108 REAL NOT NULL,
  X_109 REAL NOT NULL,
  X_110 REAL NOT NULL,
  X_111 REAL NOT NULL,
  X_112 REAL NOT NULL,
  X_113 REAL NOT NULL,
  X_114 REAL NOT NULL,
  X_115 REAL NOT NULL,
  X_116 REAL NOT NULL,
  X_117 REAL NOT NULL,
  X_118 REAL NOT NULL,
  X_119 REAL NOT NULL,
  X_120 REAL NOT NULL,
  X_121 REAL NOT NULL,
  X_122 REAL NOT NULL,
  X_123 REAL NOT NULL,
  X_124 REAL NOT NULL,
  X_125 REAL NOT NULL,
  X_126 REAL NOT NULL,
  X_127 REAL NOT NULL,
  X_128 REAL NOT NULL,
  X_129 REAL NOT NULL,
  X_130 REAL NOT NULL,
  X_131 REAL NOT NULL,
  X_132 REAL NOT NULL,
  X_133 REAL NOT NULL,
  X_134 REAL NOT NULL,
  X_135 REAL NOT NULL,
  X_136 REAL NOT NULL,
  X_137 REAL NOT NULL,
  X_138 REAL NOT NULL,
  X_139 REAL NOT NULL,
  X_140 REAL NOT NULL,
  X_141 REAL NOT NULL,
  X_142 REAL NOT NULL,
  X_143 REAL NOT NULL,
  X_144 REAL NOT NULL,
  X_145 REAL NOT NULL,
  X_146 REAL NOT NULL,
  X_147 REAL NOT NULL,
  X_148 REAL NOT NULL,
  X_149 REAL NOT NULL,
  X_150 REAL NOT NULL,
  X_151 REAL NOT NULL,
  X_152 REAL NOT NULL,
  X_153 REAL NOT NULL,
  X_154 REAL NOT NULL,
  X_155 REAL NOT NULL,
  X_156 REAL NOT NULL,
  X_157 REAL NOT NULL,
  X_158 REAL NOT NULL,
  X_159 REAL NOT NULL,
  X_160 REAL NOT NULL,
  X_161 REAL NOT NULL,
  X_162 REAL NOT NULL,
  X_163 REAL NOT NULL,
  X_164 REAL NOT NULL,
  X_165 REAL NOT NULL,
  X_166 REAL NOT NULL,
  X_167 REAL NOT NULL,
  X_168 REAL NOT NULL,
  X_169 REAL NOT NULL,
  X_170 REAL NOT NULL,
  X_171 REAL NOT NULL,
  X_172 REAL NOT NULL,
  X_173 REAL NOT NULL,
  X_174 REAL NOT NULL,
  X_175 REAL NOT NULL,
  X_176 REAL NOT NULL,
  X_177 REAL NOT NULL,
  X_178 REAL NOT NULL,
  X_179 REAL NOT NULL,
  X_180 REAL NOT NULL,
  X_181 REAL NOT NULL,
  X_182 REAL NOT NULL,
  X_183 REAL NOT NULL,
  X_184 REAL NOT NULL,
  X_185 REAL NOT NULL,
  X_186 REAL NOT NULL,
  X_187 REAL NOT NULL,
  X_188 REAL NOT NULL,
  X_189 REAL NOT NULL,
  X_190 REAL NOT NULL,
  X_191 REAL NOT NULL,
  X_192 REAL NOT NULL,
  X_193 REAL NOT NULL,
  X_194 REAL NOT NULL,
  X_195 REAL NOT NULL,
  X_196 REAL NOT NULL,
  X_197 REAL NOT NULL,
  X_198 REAL NOT NULL,
  X_199 REAL NOT NULL,
  X_200 REAL NOT NULL,
  X_201 REAL NOT NULL,
  X_202 REAL NOT NULL,
  X_203 REAL NOT NULL,
  X_204 REAL NOT NULL,
  X_205 REAL NOT NULL,
  X_206 REAL NOT NULL,
  X_207 REAL NOT NULL,
  X_208 REAL NOT NULL,
  X_209 REAL NOT NULL,
  X_210 REAL NOT NULL,
  X_211 REAL NOT NULL,
  X_212 REAL NOT NULL,
  X_213 REAL NOT NULL,
  X_214 REAL NOT NULL,
  X_215 REAL NOT NULL,
  X_216 REAL NOT NULL,
  X_217 REAL NOT NULL,
  X_218 REAL NOT NULL,
  X_219 REAL NOT NULL,
  X_220 REAL NOT NULL,
  X_221 REAL NOT NULL,
  X_222 REAL NOT NULL,
  X_223 REAL NOT NULL,
  X_224 REAL NOT NULL,
  X_225 REAL NOT NULL,
  X_226 REAL NOT NULL,
  X_227 REAL NOT NULL,
  X_228 REAL NOT NULL,
  X_229 REAL NOT NULL,
  X_230 REAL NOT NULL,
  X_231 REAL NOT NULL,
  X_232 REAL NOT NULL,
  X_233 REAL NOT NULL,
  X_234 REAL NOT NULL,
  X_235 REAL NOT NULL,
  X_236 REAL NOT NULL,
  X_237 REAL NOT NULL,
  X_238 REAL NOT NULL,
  X_239 REAL NOT NULL,
  X_240 REAL NOT NULL,
  X_241 REAL NOT NULL,
  X_242 REAL NOT NULL,
  X_243 REAL NOT NULL,
  X_244 REAL NOT NULL,
  X_245 REAL NOT NULL,
  X_246 REAL NOT NULL,
  X_247 REAL NOT NULL,
  X_248 REAL NOT NULL,
  X_249 REAL NOT NULL,
  X_250 REAL NOT NULL,
  X_251 REAL NOT NULL,
  X_252 REAL NOT NULL,
  X_253 REAL NOT NULL,
  X_254 REAL NOT NULL,
  X_255 REAL NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_embeddings_interview_id ON embeddings(interview_id);
