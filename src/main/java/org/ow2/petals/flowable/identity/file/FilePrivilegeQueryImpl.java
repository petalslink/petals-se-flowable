/**
 * Copyright (c) 2019 Linagora
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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.flowable.common.engine.impl.interceptor.CommandContext;
import org.flowable.idm.api.Privilege;
import org.flowable.idm.engine.impl.PrivilegeQueryImpl;

public class FilePrivilegeQueryImpl extends PrivilegeQueryImpl {

    private static final long serialVersionUID = -1082728490171094344L;

    /**
     * Privileges by user into a map: key=user-id, value=list of privilege
     */
    private final Map<String, List<Privilege>> privilegesByUser;

    public FilePrivilegeQueryImpl(final Map<String, List<Privilege>> privilegesByUser) {
        this.privilegesByUser = privilegesByUser;
    }

    @Override
    public long executeCount(final CommandContext commandContext) {
        return executeQuery().size();
    }

    @Override
    public List<Privilege> executeList(final CommandContext commandContext) {
        return executeQuery();
    }

    protected List<Privilege> executeQuery() {
        if (this.getUserId() != null) {
            final List<Privilege> results = new ArrayList<>();
            if (this.privilegesByUser.containsKey(this.getUserId())) {
                results.addAll(this.privilegesByUser.get(this.getUserId()));
            }
            return results;
        } else {
            final List<Privilege> results = new ArrayList<>();
            for (final List<Privilege> entry : this.privilegesByUser.values()) {
                for (final Privilege privilege : entry) {
                    if (!results.contains(privilege)) {
                        results.add(privilege);
                    }
                }
            }
            return results;
        }
    }
}
