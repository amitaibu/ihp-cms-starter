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
CREATE TABLE paragraph_quotes (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    landing_page_id UUID NOT NULL,
    weight INT DEFAULT 0 NOT NULL
);
CREATE TABLE paragraph_ctas (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    landing_page_id UUID NOT NULL,
    weight INT DEFAULT 0 NOT NULL
);
CREATE TABLE paragraph_featured_articles (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    landing_page_id UUID NOT NULL,
    weight INT DEFAULT 0 NOT NULL
);
CREATE TABLE articles (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    image_url TEXT DEFAULT NULL
);
CREATE INDEX paragraph_quotes_landing_page_id_index ON paragraph_quotes (landing_page_id);
CREATE INDEX paragraph_ctas_landing_page_id_index ON paragraph_ctas (landing_page_id);
CREATE INDEX paragraph_featured_articles_landing_page_id_index ON paragraph_featured_articles (landing_page_id);
ALTER TABLE paragraph_ctas ADD CONSTRAINT paragraph_ctas_ref_landing_page_id FOREIGN KEY (landing_page_id) REFERENCES landing_pages (id) ON DELETE NO ACTION;
ALTER TABLE paragraph_featured_articles ADD CONSTRAINT paragraph_featured_articles_ref_landing_page_id FOREIGN KEY (landing_page_id) REFERENCES landing_pages (id) ON DELETE NO ACTION;
ALTER TABLE paragraph_quotes ADD CONSTRAINT paragraph_quotes_ref_landing_page_id FOREIGN KEY (landing_page_id) REFERENCES landing_pages (id) ON DELETE NO ACTION;
