/**
 * Copyright (c) 2015-2016 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.incoming.integration;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.pool.BasePoolableObjectFactory;
import org.apache.commons.pool.PoolableObjectFactory;

/**
 * The {@link PoolableObjectFactory} to create JAXB unmarshalers
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class UnmarshalerFactory extends BasePoolableObjectFactory<Unmarshaller> {

    private final JAXBContext jaxbContext;

    public UnmarshalerFactory(final JAXBContext jaxbContext) {
        this.jaxbContext = jaxbContext;
    }

    @Override
    public Unmarshaller makeObject() throws Exception {
        return jaxbContext.createUnmarshaller();
    }
}