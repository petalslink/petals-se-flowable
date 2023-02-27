/**
 * Copyright (c) 2019-2023 Linagora
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
package org.ow2.petals.flowable.identity.file;

import org.flowable.idm.engine.IdmEngineConfiguration;
import org.flowable.ldap.LdapIdmEngineConfiguration;

/**
 * Lightweight {@link IdmEngineConfiguration} to be used when running with a file-based identity service.
 * 
 * Inspired by {@link LdapIdmEngineConfiguration}
 * 
 * @author Christophe DENEUX - Linagora
 */
public class FileIdmEngineConfiguration extends IdmEngineConfiguration {

    public FileIdmEngineConfiguration() {
        this.setUsingRelationalDatabase(false);
    }

    @Override
    public void initDataManagers() {
        // No need to initialize data managers when using a file-based identity service
    }

}
