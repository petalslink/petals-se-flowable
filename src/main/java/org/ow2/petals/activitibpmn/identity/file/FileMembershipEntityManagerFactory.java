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
package org.ow2.petals.activitibpmn.identity.file;

import org.activiti.engine.impl.interceptor.Session;
import org.activiti.engine.impl.interceptor.SessionFactory;
import org.activiti.engine.impl.persistence.entity.MembershipEntity;
import org.activiti.engine.impl.persistence.entity.MembershipIdentityManager;

/**
 * {@link SessionFactory} responsible for creating a {@link Session} that manages {@link MembershipEntity}s.
 * 
 * For file-based identity service, this will not do anything and even throw an exception when trying to use, as
 * memberships are managed by the file-based identity service itself.
 * 
 * @author Christophe DENEUX - Linagora
 */
public class FileMembershipEntityManagerFactory implements SessionFactory {

    @Override
    public Class<?> getSessionType() {
        return MembershipIdentityManager.class;
    }

    @Override
    public Session openSession() {
        throw new UnsupportedOperationException("Memberships are not supported in file-based identity service");
    }

}
