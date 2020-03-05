/**
 * Copyright (c) 2017-2020 Linagora
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
import org.flowable.idm.api.Group;
import org.flowable.idm.engine.impl.GroupQueryImpl;

public class FileGroupQueryImpl extends GroupQueryImpl {

    private static final long serialVersionUID = 5582339899992117499L;

    /**
     * Groups by user into a map: key=user-id, value=list of group-id
     */
    private final Map<String, List<Group>> groupsByUser;

    public FileGroupQueryImpl(final Map<String, List<Group>> groupsByUser) {
        this.groupsByUser = groupsByUser;
    }

    @Override
    public long executeCount(final CommandContext commandContext) {
        return executeQuery().size();
    }

    @Override
    public List<Group> executeList(final CommandContext commandContext) {
        return executeQuery();
    }

    protected List<Group> executeQuery() {
        if (this.getUserId() != null) {
            final List<Group> results = new ArrayList<>();
            if (this.groupsByUser.containsKey(this.getUserId())) {
                results.addAll(this.groupsByUser.get(this.getUserId()));
            }
            return results;
        } else {
            final List<Group> results = new ArrayList<>();
            for (final List<Group> entry : this.groupsByUser.values()) {
                for (final Group group : entry) {
                    if (!results.contains(group)) {
                        results.add(group);
                    }
                }
            }
            return results;
        }
    }
}
