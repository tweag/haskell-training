CREATE TABLE questionnaire
( id UUID PRIMARY KEY
, title TEXT
);

CREATE TABLE question
( id UUID PRIMARY KEY
, questionnaire_id UUID REFERENCES questionnaire (id) ON DELETE CASCADE ON UPDATE CASCADE
, title TEXT
, answer_type TEXT
);

CREATE TABLE answer
( id UUID PRIMARY KEY
, question_id UUID REFERENCES question (id) ON DELETE CASCADE ON UPDATE CASCADE
, submission_id UUID NOT NULL
, content TEXT
);
