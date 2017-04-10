/**
 * Copyright (c) 2015-2017 Linagora
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
package org.ow2.petals.flowable.identity;

import java.io.File;

import org.ow2.petals.flowable.identity.exception.IdentityServiceInitException;

/**
 * Interface of the identity service used by the Petals SE Flowable.
 * 
 * @author Christophe DENEUX - Linagora
 */
public interface IdentityService {

    /**
     * Initialize the identity service.
     * 
     * @param configurationFile
     *            The configuration file of the identity service. If <code>not null</code>, the file must exist and must
     *            be an absolute valid file. If <code>null</code>, a default configuration embedded into the
     *            implementation of the identity service will be used.
     */
    public void init(final File configurationFile) throws IdentityServiceInitException;

    /**
     * @return The Flowable ({@link org.flowable.engine.IdentityService}) associated to the identity service
     */
    public org.flowable.engine.IdentityService getIdentityService();

}
