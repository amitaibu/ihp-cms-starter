CREATE FUNCTION set_updated_at_to_now() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language plpgsql;
-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE landing_pages (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX landing_pages_created_at_index ON landing_pages (created_at);
CREATE TRIGGER update_landing_pages_updated_at BEFORE UPDATE ON landing_pages FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
CREATE TABLE paragraph__quotes (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    landing_page_id UUID NOT NULL,
    weight INT DEFAULT 0 NOT NULL
);
CREATE TABLE paragraph__ctas (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    landing_page_id UUID NOT NULL,
    weight INT DEFAULT 0 NOT NULL
);
CREATE TABLE paragraph__featured_articles (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    landing_page_id UUID NOT NULL,
    weight INT DEFAULT 0 NOT NULL
);
CREATE TABLE articles (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    Title TEXT NOT NULL
);
CREATE INDEX paragraph__quotes_landing_page_id_index ON paragraph__quotes (landing_page_id);
CREATE INDEX paragraph__ctas_landing_page_id_index ON paragraph__ctas (landing_page_id);
CREATE INDEX paragraph__featured_articles_landing_page_id_index ON paragraph__featured_articles (landing_page_id);
ALTER TABLE paragraph__ctas ADD CONSTRAINT paragraph__ctas_ref_landing_page_id FOREIGN KEY (landing_page_id) REFERENCES landing_pages (id) ON DELETE NO ACTION;
ALTER TABLE paragraph__featured_articles ADD CONSTRAINT paragraph__featured_articles_ref_landing_page_id FOREIGN KEY (landing_page_id) REFERENCES landing_pages (id) ON DELETE NO ACTION;
ALTER TABLE paragraph__quotes ADD CONSTRAINT paragraph__quotes_ref_landing_page_id FOREIGN KEY (landing_page_id) REFERENCES landing_pages (id) ON DELETE NO ACTION;
