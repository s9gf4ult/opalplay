CREATE EXTENSION "uuid-ossp" ;

CREATE TABLE orders
  ( id uuid primary key DEFAULT uuid_generate_v4()
  , "name" text
  , created_at timestamptz not null default now()
  ) ;

CREATE TABLE passengers
  ( id uuid primary key DEFAULT uuid_generate_v4()
  , "name" text
  , created_at timestamptz not null default now()
  ) ;

CREATE TABLE products
  ( id uuid primary key DEFAULT uuid_generate_v4()
  , "name" text
  , order_id uuid not null references orders(id)
  ,
  , product_type text not null
  , product_id uuid not null
  , created_at timestamptz not null default now()
  ) ;

CREATE TABLE airticket_product
  ( id uuid primary key DEFAULT uuid_generate_v4()
  , "name" text not null
  , created_at timestamptz not null default now()
  ) ;

CREATE TABLE surchage_product
  ( id uuid primary key DEFAULT uuid_generate_v4()
  , "name" text not null
  , created_at timestamptz not null default now()
  ) ;
